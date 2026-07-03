module Main where

import PNMLParser
import PetriNet (explorePetri)
import Explorer (deadlocks)
import LTL (normalize)
import Text.Parsec.Prim (parse)
import Data.Foldable (Foldable(toList))
import LTLGNBA (fromLTL)
import GNBA (gnbaAccepting)
import NBA (nbaFromGnba)
import Dot (showNoQuotes)
import qualified Data.Set as Set
import Pipeline (mccNBACheck)
import ParityGames.ParityArena (ParityGame(), ParityArena)
import ParityGames.ProgressMeasures (spm)
import ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj)
import ParityGames.Zielonka
import LTLXML
import HOA (HOA(toHOA))
import System.Environment (getArgs)
import ParityGames.ParityParser (parityPrefixParser, parityArenaParser)
import qualified Data.Map as Map
import ParityGames.ForcedPath (forcedPathZielonka)
import qualified Data.Foldable as Set
import System.Exit (exitSuccess)
import LTL (parseLTL)
import Data.Map.Lazy (keys)
import Dot

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

oinkSolve :: FilePath -> FilePath -> 
             (ParityArena -> (Set.Set Int, Set.Set Int, Set.Set (Int,Int), Set.Set (Int,Int))) -> IO ()
oinkSolve fIn fOut solver = do
  firstLine <- readFile fIn
  let otherLines = concat (tail (lines firstLine))
  let paritySize = case parse parityPrefixParser "" firstLine of
                        Right parsedSize -> parsedSize
                        Left errorMsg -> error ("parsing of prefix went stucky wucky: "++show errorMsg)
  let parsedGame = case parse (parityArenaParser paritySize) "" otherLines of
                        Right parsed -> parsed
                        Left errorMsg -> error ("parsing of parity game failed: "++show errorMsg)
  let (w0,w1,strat0,strat1) = solver parsedGame
  let newSet = Set.map (\x -> (x,0,fmap snd $ Set.find (\(l,_) -> l==x) strat0)) w0 <>
               Set.map (\x -> (x,1,fmap snd $ Set.find (\(l,_) -> l==x) strat1)) w1
  let outputStr =  foldMap (\(x,n,s) -> case s of
                            Nothing -> show x++" "++show n++"\n"
                            Just s' -> show x++" "++show n++" "++show s'++"\n") newSet
  writeFile fOut outputStr

main :: IO ()
main = do
  args <- getArgs
  if null args then putStrLn "please give an argument or write --help for an explanation" >> exitSuccess else pure ()
  let (arg1:args') = args
  if arg1 == "--help" 
    then putStrLn $ "This program may be invoked with the following options:\n"
                 ++ " - oink <solver name> <inputFile> <outputFile> where <solver name> is one of the following: "
                 ++ show (keys solverMap)
                 ++ "\nThe oink option is intended to be used with test_solvers from the oink parity game solver"
                 ++ ", for example one would run \"./test_solvers -e <path to this executable> zielonka %I %O ../tests\".\n"
                 ++ " - pnml <input file>\npnml with only one file given will try to parse the input file as a PNML file"
                 ++ "and calculate both the amount of reachable nodes and give the reachable deadlocks of the petri net.\n"
                 ++ " - pnml <input file> <ltl file>\nThis setting will read the first file given as a PNML file and check"
                 ++ "the LTL formulas given in the second file in mcc format against said petri net.\n"
                 ++ " - pnf <optional input file>\nThe pnf option converts an ltl formula/ltl formulae to positive normal form"
                 ++ "when not given a file in mcc ltl format, the program will wait for the user to input a ltl formula in human"
                 ++ "readable form. In this form, propositions are strings that are not one of the LTL operators."
                 ++ "For example, \"A W B U X c & !D\" is a valid formula.\n"
                 ++ " - gnbaltl <optional input file>\nThe gnabltl option will convert an ltl formula to a GNBA in HOA format."
                 ++ "The gnbaltl option takes input in the same way as the pnf option.\n"
                 ++ " - nbaltl <optional input file>\nThe gnabltl option will convert an ltl formula to a GNBA in HOA format."
                 ++ "The nbaltl option takes input in the same way as the pnf option.\n"
                 ++ " - ltlsatisfiable <optional input file>\nThe ltlsatisfiable option will check whether a given ltl formula "
                 ++ "is satisfiable. "
                 ++ "The ltlsatisfiable option takes input in the same way as the pnf option.\n"
                 ++ " - There are also the variants pnfvis, gnbaltlvis and nbaltlvis, which will write a dot file (with .gv) "
                 ++ "extension to ltl.gv, gnba.gv and nba.gv respectively. Currently there is no support for changing where "
                 ++ "the file gets written to (unless I ended up having time to add such support but forgot to update --help)."
    else pure ()
  if arg1 == "oink" 
    then 
    let (arg2:fileIn:fileOut:_) = if length args < 3
        then error "insufficient arguments provided, expected \"oink\" with a solver name and input and output file"
        else args' in oinkSolve fileIn fileOut (case solverMap Map.!? arg2 of
      Just solver -> solver
      Nothing -> error ("could not find solver "++arg2++"\nvalid solvers are "++show (Map.keys solverMap)) )
    else pure ()
  if arg1 == "pnml"
    then (if null args' then error "no PNML file was given to read"
        else if length args' == 1 then do 
          pnmltxt <- readFile (head args')
          let petri = parsePNML pnmltxt
          let numMarkings = length $ explorePetri petri
          let deadlockSet = deadlocks petri
          putStrLn ("number of reachable marking: "++show numMarkings++"\ndeadlocks found: "++show deadlockSet)
         else do
          let (arg2:arg3:_) = args'
          pnmltxt <- readFile arg2
          let petri = parsePNML pnmltxt
          ltltxt <- readFile arg3
          let ltlTerms = map (\x -> (x,mccNBACheck petri x)) $ parseLTLXMLFireability ltltxt
          putStrLn (foldMap (\(l,r) -> "- "++showNoQuotes (fmap showFireCard l)++": "++show r++"\n") ltlTerms)
          )
    else pure ()
  if arg1 == "pnf" || arg1 == "pnfvis"
    then (if null args' then do
              inputLTL <- getLine
              let ltl = case parseLTL inputLTL of
                             Left errorMSG -> error (show errorMSG)
                             Right x -> x
              print (normalize ltl)
              if arg1 == "pnfvis" then dotWrite (pure "ltl.gv") (normalize ltl) else pure ()
          else do
            inputLTL <- readFile (head args')
            let ltl = parseLTLXMLFireability inputLTL
            let ltlNormalized = (map (\x -> (x,normalize x)) ltl)  
            putStrLn (foldMap (\(l,r) -> "-  "++showNoQuotes (fmap showFireCard l)++"\n-> "++
                                         showNoQuotes (fmap showFireCard r)++"\n") ltlNormalized))
    else pure ()
    
  if arg1 == "gnbaltl" || arg1 == "gnbaltlvis"
    then (if null args' then do
              inputLTL <- getLine
              let ltl = case parseLTL inputLTL of
                             Left errorMSG -> error (show errorMSG)
                             Right x -> x
              let ltlGNBA = fromLTL ltl
              putStrLn (toHOA ltlGNBA (showNoQuotes ltl))
              if arg1 == "gnbaltlvis" then dotWrite (pure "gnba.gv") ltlGNBA else pure ()
          else do
            inputLTL <- readFile (head args')
            let ltl = parseLTLXMLFireability inputLTL
            let ltlGNBA = (map (\x -> (toHOA (fromLTL x) (showNoQuotes (fmap showFireCard x)))) ltl)  
            putStrLn (foldMap (\l -> l++"\n") ltlGNBA))
    else pure ()
      
  if arg1 == "nbaltl" || arg1 == "nbaltlvis"
    then (if null args' then do
              inputLTL <- getLine
              let ltl = case parseLTL inputLTL of
                             Left errorMSG -> error (show errorMSG)
                             Right x -> x
              let ltlGNBA = nbaFromGnba $ fromLTL ltl
              putStrLn (toHOA ltlGNBA (showNoQuotes ltl))
              if arg1 == "nbaltlvis" then dotWrite (pure "nba.gv") ltlGNBA else pure ()
          else do
            inputLTL <- readFile (head args')
            let ltl = parseLTLXMLFireability inputLTL
            let ltlGNBA = map (\x -> (toHOA (nbaFromGnba $ fromLTL x) (showNoQuotes (fmap showFireCard x)))) ltl 
            putStrLn (foldMap (\l -> l++"\n") ltlGNBA))
    else pure ()
  
  if arg1 == "ltlsatisfiable" 
    then 
      if null args' then do
          inputLTL <- getLine
          let ltl = case parseLTL inputLTL of
                          Left errorMSG -> error (show errorMSG)
                          Right x -> x
          let ltlGNBA = fromLTL ltl 
          if gnbaAccepting ltlGNBA 
            then putStrLn (showNoQuotes ltl++" is satisfiable") 
            else putStrLn (showNoQuotes ltl++" is not satisfiable")
        else do
            inputLTL <- readFile (head args')
            let ltl = parseLTLXMLFireability inputLTL
            let ltlGNBA = map (\x -> (x,fromLTL x)) ltl
            putStrLn (foldMap (\(ltl',x) -> 
              if gnbaAccepting x 
                then ("- "++showNoQuotes ltl'++" is satisfiable\n") 
                else ("- "++showNoQuotes ltl'++" is not satisfiable\n")) ltlGNBA)
    else pure ()
  exitSuccess


solverMap :: Map.Map String (ParityGame a -> (Set.Set Int, Set.Set Int, Set.Set (Int, Int), Set.Set (Int, Int)))
solverMap = Map.fromList [
                         ("fpj",fpj), 
                         ("fpi",(\pa -> let (w0,w1) = fpi pa in (w0,w1,Set.empty,Set.empty))),
                         ("fpiFreeze",fpiFreeze), 
                         ("zielonka",zielonkaStrat),
                         ("spm", spm),
                         ("zielonkaPaths", forcedPathZielonka)
                         ]

dotWrite :: Dot a => IO String -> a -> IO ()
dotWrite getStr automata = do
                        location <- getStr
                        writeFile location (genDot automata)

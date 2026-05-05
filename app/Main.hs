module Main where

import PNMLParser
import PetriNet (explorePetri, prettyPetriState)
import Explorer (explore, deadlocks)
import qualified Data.Set
import DiningPhilosophers (LeftForkFirst(LeftForkFirst), ArbitraryFork (ArbitraryFork), CrazyFork (CrazyFork))
import Text.Parsec (runParser, parseTest)
import LTL (ltlParser, closure, normalize, consistentSubsetsLTL, getAtomics)
import Text.Parsec.Prim (parse)
import Data.Either (fromRight)
import Data.Foldable (Foldable(toList))
import LTLGNBA (fromLTL)
import GNBA (gnbaBimap, GNBA (transitionsGNBA))
import NBA (nbaFromGnba, NBA (transitionsNBA))
import Dot (genDot)

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

main :: IO ()
main = do
  ltlString <- getLine
  let ltl = fromRight (error "parsing LTL failed") $ parse ltlParser "" ltlString
  let ltlDot = genDot ltl
  writeFile "ltlDot.gv" ltlDot
  putStrLn ("normalize:\n"++ show (normalize ltl))
  putStrLn ("n2:\n"++ show (normalize (normalize ltl)))
  putStrLn "G F !((true M (false R -4)) <-> ((12 & 15) W G true))"

{-
  print ltl
  print (normalize ltl)
  print (closure (normalize ltl))
  let consistent = (consistentSubsetsLTL (closure (normalize ltl)))
  prettySet consistent
  print (length consistent)
  putStrLn ("atomics: " ++ show (getAtomics ltl))
  let ltlgnba = gnbaBimap toList toList (fromLTL ltl)
  print ltlgnba
  print (length (transitionsGNBA ltlgnba))
  let ltlnba = nbaFromGnba ltlgnba
  print ltlnba
  print (length (transitionsNBA ltlnba))
  let ltlgnbaDot = genDot ltlgnba
  writeFile "gnbaDot.gv" ltlgnbaDot
  let ltlnbaDot = genDot ltlnba
  writeFile "nbaDot.gv" ltlnbaDot
  -}
  {-
  let filesrc = "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\refFiles\\philoslides.pnml"
  txt <- readFile filesrc
  let petri = parsePNML txt
  print (Data.Set.fromList (explorePetri petri) == explore petri)
  let x = Data.Set.fromList $ explorePetri petri
  let y = explore (LeftForkFirst 4)
  putStrLn $ concatMap (\s -> s++"\n") $ prettyPetriState <$> Data.Set.toList x
  putStrLn $ concatMap (\s -> show s++"\n") $ Data.Set.toList y
  let petriDot = genDot petri 
  writeFile "petri.gv" petriDot
  -}
  -- print (length (explore (LeftForkFirst 11)))




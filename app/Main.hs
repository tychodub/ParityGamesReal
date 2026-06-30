{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import PNMLParser
import PetriNet (explorePetri, prettyPetriState)
import Explorer (explore, deadlocks, tarjanNontrivial)
import qualified Data.Set
import DiningPhilosophers (LeftForkFirst(LeftForkFirst), ArbitraryFork (ArbitraryFork), CrazyFork (CrazyFork))
import Text.Parsec (runParser, parseTest)
import LTL (ltlParser, closure, normalize, consistentSubsetsLTL, getAtomics, LTL (LTNot, LTTerm, LTU), ltlParserInt)
import Text.Parsec.Prim (parse)
import Data.Either (fromRight)
import Data.Foldable (Foldable(toList))
import LTLGNBA (fromLTL)
import GNBA (gnbaBimap, GNBA (transitionsGNBA), initialGNBA, acceptingGNBA)
import NBA (nbaFromGnba, NBA (transitionsNBA, initialNBA), tsMul, dfsLasso, trimNBA)
import Dot (genDot)
import TS (completeTS, discreteTS, TS (tsInitial), tsParser, fromPetri)
import qualified Data.Set as Set
import qualified GNBA
import qualified Data.Graph as Graph
import Pipeline (nbaLTLCheck, gnbaLTLCheck, nbaLTLCheck2, mccNBACheck)
import qualified Data.Graph as Graph
import ParityGames.ParityArena (ParityGame(ArenaPA), subGame, pruneLeafs, flatPA, ParityArena, Player(..))
import ParityGames.ProgressMeasures (llsFromPA, gazdaWillemseSPMPartition, spmSlides, spm)
import ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj)
import qualified GHC.Arr as Arr
import ParityGames.Zielonka
import ParityGames.TangleLearning
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromJust)
import LTLXML
import HOA (HOA(toHOA))
import LTL (LTL(..))
import System.Environment (getArgs)
import Data.List (stripPrefix)
import Text.Parsec.String (Parser)
import ParityGames.ParityParser (parityPrefixParser, parityArenaParser)
import qualified Data.Map as Map
import ParityGames.ForcedPath (forcedPathZielonka)
import qualified System.IO
import qualified Data.Foldable as Set

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

-- try: G ((false -> true) R (true -> false))
main :: IO ()
main = do
  args <- getArgs
  let (arg1:arg2:fileIn:fileOut:_) = if length args < 4 
      then error "insufficient arguments provided, expected \"oink\" with a solver name and input and output file"
      else args
  if arg1 == "oink" 
    then case solverMap Map.!? arg2 of
      Just solver -> oinkSolve fileIn fileOut solver
      Nothing -> error ("could not find solver "++arg2++"\nvalid solvers are "++show (Map.keys solverMap)) 
    else error "first arg was not a valid option"

solverMap :: Map.Map String (ParityGame a -> (Set.Set Int, Set.Set Int, Set.Set (Int, Int), Set.Set (Int, Int)))
solverMap = Map.fromList [
                         ("fpj",fpj), -- possibly multiple strat per node
                         ("fpi",(\pa -> let (w0,w1) = fpi pa in (w0,w1,Set.empty,Set.empty))),
                         ("fpiFreeze",fpiFreeze), -- possibly multiple strat per node
                         ("zielonka",zielonkaStrat),
                         ("spm", spm),
                         ("zielonkaPaths", forcedPathZielonka)
                         ]
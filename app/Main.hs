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
import Pipeline (nbaLTLCheck, gnbaLTLCheck, nbaLTLCheck2)
import qualified Data.Graph as Graph
import ParityGames.ParityArena (ParityGame(ArenaPA), subGame, pruneLeafs, flatPA, ParityArena, Player(..))
import ParityGames.ProgressMeasures (llsFromPA, gazdaWillemseSPMPartition, spmSlides)
import ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj)
import qualified GHC.Arr as Arr
import ParityGames.Zielonka
import ParityGames.TangleLearning
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromJust)
import LTLXML
import HOA (HOA(toHOA))
import LTL (LTL(..))

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

-- try: G ((false -> true) R (true -> false))
main :: IO ()
main = do
  ltlxml <- readFile "refFiles/LTLCardinality.xml"
  let ltltmp = parseLTLXMLFireability ltlxml
  print ltltmp
  pnmlModelxml <- readFile "refFiles/model copy.pnml"
  let model = parsePNML pnmlModelxml -- PNML IS SUS
  print (model)
  print (fromPetri model (getAtomics (head ltltmp)))
  writeFile "dotfiles/mccCardModelCopy.gv" (genDot (fromPetri model (getAtomics (head ltltmp))))
  --let graph = Arr.array (0,1) [(0,[0,1]),(1,[0])]
  --let pa = ArenaPA graph id even id
  --let paTrivial = flatPA (Arr.array (0,2) [(0,[1]),(1,[0]),(2,[1])])
  --let graph2 = Arr.array (0,10) [(0,[4]),(1,[0]),(2,[3]),(3,[8]),(4,[5]),(5,[4]),(6,[4]),(7,[5]),(8,[9]),(9,[10]),(10,[3])]
  --let pa2 = ArenaPA graph2 id even id
  ltlString <- getLine
  let ltl = case parse ltlParser "" ltlString of
                 Left x -> error (show x)
                 Right x -> x
  let ts = completeTS (Set.fromList ["sleep", "eat", "repeat"]) (Set.fromList ["sleep"]) 
                      (\x -> 
                        if x == "sleep" then (Set.singleton "a") else (Set.singleton  "b")
                      )
  let tsDot = genDot ts
  writeFile "dotfiles/ts.gv" tsDot
  let ts2 = discreteTS (Set.fromList ["sleep", "eat", "repeat", "otherwise"]) (Set.fromList ["sleep", "otherwise"]) 
                      (\x -> if x == "sleep" then (Set.singleton "a") else (Set.singleton  "b"))
  let ts2Dot = genDot ts2
  writeFile "dotfiles/ts2.gv" ts2Dot
  putStrLn "ts1:"
  print $ nbaLTLCheck ts ltl
  print $ nbaLTLCheck2 ts ltl
  print $ gnbaLTLCheck ts ltl
  putStrLn "ts2:"
  print $ gnbaLTLCheck ts2 ltl
  print $ nbaLTLCheck ts2 ltl
  let ltlgnba = fromLTL (LTNot ltl)
  let atomics = getAtomics (LTNot ltl)
  let ltlnba = nbaFromGnba ltlgnba
  let ltlgnbaDot = genDot ltlgnba
  writeFile "dotfiles/gnbaDot.gv" ltlgnbaDot
  let ltlnbaDot = genDot ltlnba
  writeFile "dotfiles/nbaDot.gv" ltlnbaDot
  let tsTensorNBA = tsMul ts ltlnba atomics
  let tsTensorNBADot = genDot tsTensorNBA
  writeFile "dotfiles/tsNBATensor.gv" tsTensorNBADot
  let ts2TensorNBA = tsMul ts2 ltlnba atomics
  let ts2TensorNBADot = genDot ts2TensorNBA
  writeFile "dotfiles/ts2NBATensor.gv" ts2TensorNBADot
  writeFile "dotfiles/ts2NBATensorTrimmed.gv" (genDot (trimNBA ts2TensorNBA))
  let tsTensorGNBA = GNBA.tsMul ts ltlgnba atomics
  let tsTensorGNBADot = genDot tsTensorGNBA
  writeFile "dotfiles/tsGNBATensor.gv" tsTensorGNBADot
  let ts2TensorGNBA = GNBA.tsMul ts2 ltlgnba atomics
  let ts2TensorGNBADot = genDot ts2TensorGNBA
  writeFile "dotfiles/ts2GNBATensor.gv" ts2TensorGNBADot
  {-
  ts3txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\refFiles\\explodingTest"
  let ts3Parsed = parse tsParser "" ts3txt
  let ts3 = fromRight (error (show ts3Parsed)) ts3Parsed
  let ts3Dot = genDot ts3
  writeFile "dotfiles/tsExploding.gv" ts3Dot
  let ltlExplodeParsed = parse ltlParserInt "" "(-5 M (false <-> false))"
  let ltlExplode = fromRight (error (show ltlExplodeParsed)) $ ltlExplodeParsed
  let gnbaExplode = fromLTL (LTNot ltlExplode)
  let nbaExplode = nbaFromGnba gnbaExplode
  writeFile "dotfiles/gnbaExplode.gv" (genDot gnbaExplode)
  writeFile "dotfiles/nbaExplode.gv" (genDot nbaExplode)
  putStrLn "explode check:"
  print (nbaLTLCheck ts3 ltlExplode)
  print (nbaLTLCheck2 ts3 ltlExplode)
  print (gnbaLTLCheck ts3 ltlExplode)
  let mulExplode = (((tsMul ts3 (nbaFromGnba gnbaExplode) (getAtomics ltlExplode))))
  let mulGNBAExplode = (((GNBA.tsMul ts3 gnbaExplode (getAtomics ltlExplode))))
  writeFile "dotfiles/mulExplode.gv" (genDot mulExplode)
  writeFile "dotfiles/mulGNBAExplode.gv" (genDot mulGNBAExplode)
  -}
  
  -- print (length (explore (LeftForkFirst 11)))

commonSlidePG :: ParityGame Char
commonSlidePG = ArenaPA (Arr.array (0,8) [(0,[1]),(1,[0,5]),(2,[1,6]),(3,[2,4]),(4,[3,8]),
                                          (5,[6]),(6,[7]),(7,[3,8]),(8,[4,7])]) prio owns nodeVisual
    where
      prio 0 = 0
      prio 1 = 2
      prio 2 = 7
      prio 3 = 1
      prio 4 = 5
      prio 5 = 8
      prio 6 = 6
      prio 7 = 2
      prio 8 = 3
      prio n = error ("common slide PA only has 9 nodes, got value: "++show n) 
      owns n = not (n == 1 || n == 3)
      nodeVisual n = ['a'..'i']!!n



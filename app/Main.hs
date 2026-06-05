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
import TS (completeTS, discreteTS, TS (tsInitial), tsParser)
import qualified Data.Set as Set
import qualified GNBA
import qualified Data.Graph as Graph
import Pipeline (nbaLTLCheck, gnbaLTLCheck, nbaLTLCheck2)
import qualified Data.Graph as Graph
import ParityGames.ParityArena (ParityGame(ArenaPA), subGame, pruneLeafs, tangleLearning, flatPA)
import ParityGames.ProgressMeasures (spmBasic, llsFromPA, smallRange, gazdaWillemseSPMPartition)
import ParityGames.FixedPointSolver (fpi)
import qualified GHC.Arr as Arr
import ParityGames.Zielonka

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

-- try: G ((false -> true) R (true -> false))
main :: IO ()
main = do
  let graph = Arr.array (0,1) [(0,[0,1]),(1,[0])]
  let pa = ArenaPA graph id even id
  let paTrivial = flatPA (Arr.array (0,2) [(0,[1]),(1,[0]),(2,[1])])
  writeFile "parity1.gv" (genDot pa)
  writeFile "parityTrivial.gv" (genDot paTrivial)
  --let spm = spmBasic pa (llsFromPA pa)
  --print spm
  --let zie = zielonka pa
  --print zie
  --print (zielonkaStrat pa)
  --print (tangleLearning pa)
  putStrLn "pa1"
  print (zielonkaStrat pa)
  print (fpi pa)
  print (tangleLearning pa)
  putStrLn "paTrivial"
  print (zielonkaStrat paTrivial)
  print (fpi paTrivial)
  print (tangleLearning paTrivial)
  let graph2 = Arr.array (0,15) [(0,[15,5]),(1,[2,14]),(2,[4,15]),(3,[11,7,7]),(4,[2]),(5,[2,10,14,5]),(6,[4]),(7,[13]),(8,[11,1]),(9,[8]),(10,[15,9]),(11,[1,3]),(12,[3]),(13,[2]),(14,[9]),(15,[13,6])]
  let pa2 = ArenaPA graph2 id even id
  writeFile "parity2.gv" (genDot pa2)
  putStrLn "pa2"
  print (zielonkaStrat pa2)
  print (fpi pa2)
  print (tangleLearning pa2)
  --writeFile "parity2Pruned.gv" (genDot (pruneLeafs pa2))
  --print (zielonka pa2)
  --print (zielonkaStrat pa2)
  --let spm2 = gazdaWillemseSPMPartition pa (llsFromPA pa)
  --print spm2

  {-
  ltlString <- getLine
  let ltl = case parse ltlParser "" ltlString of
                 Left x -> error (show x)
                 Right x -> x
  let ts = completeTS (Set.fromList ["sleep", "eat", "repeat"]) (Set.fromList ["sleep"]) 
                      (\x -> 
                        if x == "sleep" then (Set.singleton "a") else (Set.singleton  "b")
                      )
  let tsDot = genDot ts
  writeFile "ts.gv" tsDot
  let ts2 = discreteTS (Set.fromList ["sleep", "eat", "repeat", "otherwise"]) (Set.fromList ["sleep", "otherwise"]) 
                      (\x -> if x == "sleep" then (Set.singleton "a") else (Set.singleton  "b"))
  let ts2Dot = genDot ts2
  writeFile "ts2.gv" ts2Dot
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
  writeFile "gnbaDot.gv" ltlgnbaDot
  let ltlnbaDot = genDot ltlnba
  writeFile "nbaDot.gv" ltlnbaDot
  let tsTensorNBA = tsMul ts ltlnba atomics
  let tsTensorNBADot = genDot tsTensorNBA
  writeFile "tsNBATensor.gv" tsTensorNBADot
  let ts2TensorNBA = tsMul ts2 ltlnba atomics
  let ts2TensorNBADot = genDot ts2TensorNBA
  writeFile "ts2NBATensor.gv" ts2TensorNBADot
  writeFile "ts2NBATensorTrimmed.gv" (genDot (trimNBA ts2TensorNBA))
  let tsTensorGNBA = GNBA.tsMul ts ltlgnba atomics
  let tsTensorGNBADot = genDot tsTensorGNBA
  writeFile "tsGNBATensor.gv" tsTensorGNBADot
  let ts2TensorGNBA = GNBA.tsMul ts2 ltlgnba atomics
  let ts2TensorGNBADot = genDot ts2TensorGNBA
  writeFile "ts2GNBATensor.gv" ts2TensorGNBADot
  -}
  {-
  ts3txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\refFiles\\explodingTest"
  let ts3Parsed = parse tsParser "" ts3txt
  let ts3 = fromRight (error (show ts3Parsed)) ts3Parsed
  let ts3Dot = genDot ts3
  writeFile "tsExploding.gv" ts3Dot
  let ltlExplodeParsed = parse ltlParserInt "" "(-5 M (false <-> false))"
  let ltlExplode = fromRight (error (show ltlExplodeParsed)) $ ltlExplodeParsed
  let gnbaExplode = fromLTL (LTNot ltlExplode)
  let nbaExplode = nbaFromGnba gnbaExplode
  writeFile "gnbaExplode.gv" (genDot gnbaExplode)
  writeFile "nbaExplode.gv" (genDot nbaExplode)
  putStrLn "explode check:"
  print (nbaLTLCheck ts3 ltlExplode)
  print (nbaLTLCheck2 ts3 ltlExplode)
  print (gnbaLTLCheck ts3 ltlExplode)
  let mulExplode = (((tsMul ts3 (nbaFromGnba gnbaExplode) (getAtomics ltlExplode))))
  let mulGNBAExplode = (((GNBA.tsMul ts3 gnbaExplode (getAtomics ltlExplode))))
  writeFile "mulExplode.gv" (genDot mulExplode)
  writeFile "mulGNBAExplode.gv" (genDot mulGNBAExplode)
  -}
  
  -- print (length (explore (LeftForkFirst 11)))




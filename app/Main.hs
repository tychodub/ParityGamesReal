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
import ParityGames.ParityArena (ParityGame(ArenaPA), subGame, pruneLeafs, flatPA, ParityArena, Player(..))
import ParityGames.ProgressMeasures (llsFromPA, gazdaWillemseSPMPartition, spmSlides)
import ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj)
import qualified GHC.Arr as Arr
import ParityGames.Zielonka
import ParityGames.TangleLearning
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromJust)

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

-- try: G ((false -> true) R (true -> false))
main :: IO ()
main = do
  let graph = Arr.array (0,1) [(0,[0,1]),(1,[0])]
  let pa = ArenaPA graph id even id
  let paTrivial = flatPA (Arr.array (0,2) [(0,[1]),(1,[0]),(2,[1])])
  writeFile "dotfiles/parity1.gv" (genDot pa)
  writeFile "dotfiles/parityTrivial.gv" (genDot paTrivial)
  writeFile "dotfiles/pa2loop.gv" (genDot (Arr.array (0 :: Int,33) [(0,[30 :: Int]),(1,[0,14]),(2,[9]),(3,[19]),(4,[30]),(5,[25,9,18]),(6,[11,9]),(7,[5]),(8,[16]),(9,[28]),(10,[28]),(11,[7]),(12,[6]),(13,[0]),(14,[20,12]),(15,[10]),(16,[22]),(17,[32]),(18,[9,15]),(19,[22]),(20,[22]),(21,[4,14]),(22,[9]),(23,[1]),(24,[13]),(25,[19,6]),(26,[18]),(27,[12]),(28,[19,23]),(29,[12,15]),(30,[12]),(31,[32]),(32,[22,11]),(33,[2])]))
  putStrLn "pa1"
  print (zielonkaStrat pa)
  print (fpiFreeze pa)
  print (fpj pa)
  print (spmSlides pa Even)
  print (tangleLearning pa)
  putStrLn "paTrivial"
  print (zielonkaStrat paTrivial)
  print (fpiFreeze paTrivial)
  print (fpj paTrivial)
  print (spmSlides paTrivial Even)
  print (tangleLearning paTrivial)
  let graph2 = Arr.array (0,10) [(0,[4]),(1,[0]),(2,[3]),(3,[8]),(4,[5]),(5,[4]),(6,[4]),(7,[5]),(8,[9]),(9,[10]),(10,[3])]
  let pa2 = ArenaPA graph2 id even id
  writeFile "dotfiles/parity2.gv" (genDot pa2)
  putStrLn "pa2"
  print (zielonkaStrat pa2)
  print (fpiFreeze pa2)
  print (fpj pa2)
  print (spmSlides pa2 Even)
  print (tangleLearning pa2)
  {-
  putStrLn "pruned pa2"
  let (pa2Pruned,nfrom,_) = pruneLeafs pa2
  let nodeFromPA2Pruned = fromJust . nfrom
  let f = (\(a,b,c,d) -> (Set.map nodeFromPA2Pruned a,Set.map nodeFromPA2Pruned b, 
                          Set.map (bimap nodeFromPA2Pruned nodeFromPA2Pruned) c,
                          Set.map (bimap nodeFromPA2Pruned nodeFromPA2Pruned) d))
  print (f $ zielonkaStrat pa2Pruned)
  print (f $ fpiFreeze pa2Pruned)
  print (f $ fpj pa2Pruned)
  -}
  putStrLn "common slide example"
  print (zielonkaStrat commonSlidePG)
  print (fpiFreeze commonSlidePG)
  print (fpj commonSlidePG)
  print (spmSlides commonSlidePG Even)
  print (tangleLearning commonSlidePG)
  writeFile "dotfiles/commonslide.gv" (genDot commonSlidePG)
  --writeFile "dotfiles/prunedPA2.gv" (genDot pa2Pruned)
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



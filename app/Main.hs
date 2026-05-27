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
import ParityArena (ParityArena(ArenaPA), subGame, zielonka)
import ProgressMeasures (spmBasic, llsFromPA, smallRange, gazdaWillemseSPMPartition)

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

-- try: G ((false -> true) R (true -> false))
main :: IO ()
main = do
  let graph = Graph.buildG (0,30) [(0,18),(1,30),(2,11),(3,7),(4,29),(5,26),(6,26),(7,6),(8,26),(9,24),(10,26),(11,3),(12,15),(13,11),(14,10),(15,0),(16,22),(17,24),(18,17),(19,6),(20,29),(21,29),(22,2),(23,20),(24,19),(25,25),(26,19),(27,4),(28,14),(29,7),(30,24)]
  let pa = ArenaPA graph id even
  writeFile "parity1.gv" (genDot pa)
  --let spm = spmBasic pa (llsFromPA pa)
  --print spm
  let zie = zielonka pa
  print zie
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




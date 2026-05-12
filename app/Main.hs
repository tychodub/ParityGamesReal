module Main where

import PNMLParser
import PetriNet (explorePetri, prettyPetriState)
import Explorer (explore, deadlocks, tarjanNontrivial)
import qualified Data.Set
import DiningPhilosophers (LeftForkFirst(LeftForkFirst), ArbitraryFork (ArbitraryFork), CrazyFork (CrazyFork))
import Text.Parsec (runParser, parseTest)
import LTL (ltlParser, closure, normalize, consistentSubsetsLTL, getAtomics, LTL (LTNot, LTTerm, LTU))
import Text.Parsec.Prim (parse)
import Data.Either (fromRight)
import Data.Foldable (Foldable(toList))
import LTLGNBA (fromLTL)
import GNBA (gnbaBimap, GNBA (transitionsGNBA), initialGNBA, acceptingGNBA)
import NBA (nbaFromGnba, NBA (transitionsNBA), tsMul, dfsLasso)
import Dot (genDot)
import TS (completeTS, discreteTS, TS (tsInitial))
import qualified Data.Set as Set
import qualified GNBA
import Pipeline (nbaLTLCheck, gnbaLTLCheck)

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn $ "consistent: " ++ (foldMap (\x -> show x++"\n") $ toList s)

main :: IO ()
main = do
  ltlString <- getLine
  let ltl = case parse ltlParser "" ltlString of
                 Left x -> error (show x)
                 Right x -> x
  let ts = completeTS (Set.fromList ["sleep", "eat", "repeat"]) (Set.fromList ["sleep"]) 
                      (\x -> Set.fromList ["b"]
                        --if x == "sleep" then Set.singleton (Set.singleton "a") else Set.singleton (Set.singleton  "b")
                      )
  let tsDot = genDot ts
  writeFile "ts.gv" tsDot
  let ts2 = discreteTS (Set.fromList ["sleep", "eat", "repeat", "otherwise"]) (Set.fromList ["sleep", "otherwise"]) 
                      (\x -> if x == "sleep" then (Set.singleton "a") else (Set.singleton  "b"))
  let ts2Dot = genDot ts2
  writeFile "ts2.gv" ts2Dot
  print $ nbaLTLCheck ts ltl
  print $ nbaLTLCheck ts2 ltl
  print $ gnbaLTLCheck ts ltl
  print $ gnbaLTLCheck ts2 ltl
  let ltlgnba = fromLTL (LTNot ltl)
  let ltlnba = nbaFromGnba ltlgnba
  let ltlgnbaDot = genDot ltlgnba
  writeFile "gnbaDot.gv" ltlgnbaDot
  let ltlnbaDot = genDot ltlnba
  writeFile "nbaDot.gv" ltlnbaDot
  let tsTensorNBA = tsMul ts ltlnba 
  let tsTensorNBADot = genDot tsTensorNBA
  writeFile "tsNBATensor.gv" tsTensorNBADot
  let ts2TensorNBA = tsMul ts2 ltlnba
  let ts2TensorNBADot = genDot ts2TensorNBA
  writeFile "ts2NBATensor.gv" ts2TensorNBADot
  let tsTensorGNBA = GNBA.tsMul ts ltlgnba
  let tsTensorGNBADot = genDot tsTensorGNBA
  writeFile "tsGNBATensor.gv" tsTensorGNBADot
  let ts2TensorGNBA = GNBA.tsMul ts2 ltlgnba
  let ts2TensorGNBADot = genDot ts2TensorGNBA
  writeFile "ts2GNBATensor.gv" ts2TensorGNBADot
  writeFile "tmpLec.gv" (genDot (fromLTL (LTU (LTTerm "a") (LTTerm "b"))))

  -- print (length (explore (LeftForkFirst 11)))




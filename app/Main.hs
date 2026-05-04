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

prettySet :: (Show a, Foldable t) => t a -> IO ()
prettySet s = putStrLn (foldMap (\x -> show x++"\n") $ toList s)

main :: IO ()
main = do
  ltlString <- getLine
  let ltl = fromRight (error "parsing LTL failed") $ parse ltlParser "" ltlString
  print ltl
  print (normalize ltl)
  print (closure (normalize ltl))
  putStrLn ""
  let consistent = (consistentSubsetsLTL (closure (normalize ltl)))
  prettySet consistent
  print (length consistent)
  print (getAtomics ltl)
  let ltlgnba = gnbaBimap toList toList (fromLTL ltl)
  print ltlgnba
  print (length (transitionsGNBA ltlgnba))
  print (nbaFromGnba ltlgnba)
  print (length (transitionsNBA (nbaFromGnba ltlgnba)))
  {-
  filesrc <- getLine
  txt <- readFile filesrc
  let petri = parsePNML txt
  print (Data.Set.fromList (explorePetri petri) == explore petri)
  --print (deadlocks petri)
  --print (deadlockPresent petri)
  print (length (explorePetri petri))
  print (length (explore (LeftForkFirst 4)))
  print (length (explore (ArbitraryFork 4)))
  print (length (explore (CrazyFork 4 0)))
  print (length (explore (CrazyFork 4 1))) -- should be the same because of symmetry
  print (length (deadlocks (LeftForkFirst 4)))
  print (length (deadlocks (ArbitraryFork 4)))
  print (length (deadlocks (CrazyFork 4 0)))
  let x = Data.Set.fromList $ explorePetri petri
  let y = explore (LeftForkFirst 4)
  putStrLn $ concatMap (\s -> s++"\n") $ prettyPetriState <$> Data.Set.toList x
  putStrLn $ concatMap (\s -> show s++"\n") $ Data.Set.toList y
  -}
  -- print (length (explore (LeftForkFirst 11)))




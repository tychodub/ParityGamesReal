{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where
import Test.QuickCheck
import LTL (normalize, LTL (..), closure, getAtomics, parseLTL)
import qualified Data.Set as Set
import Dot (showNoQuotes)
import TS
import Pipeline (nbaLTLCheck, nbaLTLCheck2, gnbaLTLCheck, reducedNBALTLCheck, reducedNBALTLCheck2)
import ParityGames.ParityArena
import ParityGames.Zielonka
import ParityGames.TangleLearning
import qualified Data.Graph
import ParityGames.ProgressMeasures (spmSlides)
import ParityGames.FixedPointSolver (fpi)
import ParityGames.FixedPointSolver (fpiFreeze, fpj)
import ParityGames.ForcedPath (forcedPathZielonka)

instance Arbitrary a => Arbitrary (LTL a) where
  arbitrary = sized ltlArb
    where
        binArb f n = (f <$> (ltlArb (n `div` 2)) <*> (ltlArb (n `div` 2)))
        ltlArb n | n < 3 = frequency [(1, pure LTTrue), (1, pure LTFalse), (1, LTTerm <$> arbitrary)]
                 | otherwise = frequency [(5,binArb LTAnd n), (5,binArb LTOr n), (5,binArb LTImpl n), (5,binArb LTEqv n),
                                          (5,binArb LTU n), (5,binArb LTW n), (5,binArb LTR n), (5,binArb LTM n),
                                          (5,LTX <$> (ltlArb (n-1))), (5,LTF <$> (ltlArb (n-1))),
                                          (5,LTG <$> (ltlArb (n-1))), (5,LTNot <$> (ltlArb (n-1))),
                                          (1, pure LTTrue), (1, pure LTFalse), (1, LTTerm <$> arbitrary)]

newtype LTLChar = LTLChar Char deriving (Show, Eq)

instance Arbitrary LTLChar where
  arbitrary = oneof $ fmap (pure . LTLChar) ['a'..'z']

sizedArb :: (Ord b, Ord a, Ord c, Arbitrary a, Arbitrary b, Arbitrary c) => Int -> Gen (TS a b c)
sizedArb n = do -- TS <$> states >>= initial <*> transitions <*> stateLabels
               sts <- states
               initsts <- initial sts
               actions' <- actions 
               transts <- transitions sts actions'
               TS sts initsts (Set.fromList actions') transts <$> (stateLabels sts)
    where
      actions = vector (min n 10)
      states = vector (max n 20)
      initial s = Set.fromList <$> (sublistOf s)
      transitions' s = sublistOf ([(l,r) | l <- s, r <- s]) 
      transitions s acts = mconcat <$> traverse (\m -> (\xs -> foldMap (\(l,r) -> 
        (\x y -> if x==l && y==m then [r] else [])) xs)<$>transitions' s) acts  
      stateLabels' s =  traverse (\z -> (\y -> (z,y)) <$> arbitrary) s
      stateLabels s = fmap listToFun (stateLabels' s)
      listToFun xs a = case lookup a xs of Just x -> x; Nothing -> Set.empty

instance (Arbitrary a, Arbitrary b, Ord a, Ord b, Ord c, Arbitrary c) => Arbitrary (TS a b c) where
  arbitrary = sized sizedArb

-- this is specifically for parity graphs, not general purpose graphs
instance Arbitrary Data.Graph.Graph where
    arbitrary = sized arbSized
        where
          arbSized n = Data.Graph.buildG (0, n) <$> edges n
          edges n = do 
            xs <- fmap (\x -> replicate x ()) arbitrary 
            es <- traverse (\x -> fmap (\y -> (x,y)) arbitrarySizedNatural) [0..n]
            es2 <- traverse (\_ -> (arbitrarySizedNatural >>= (\x -> fmap ((,) x) arbitrarySizedNatural))) xs
            pure (es <> es2)
            

instance Arbitrary ParityArena where
    arbitrary = ArenaPA <$> arbitrary <*> (pure id) <*> (pure even) <*> (pure id)     

instance Arbitrary Player where
    arbitrary = oneof [pure Even, pure Odd]

main :: IO ()
main = do 
  --graph <- generate arbitrary :: IO ParityArena
  --print graph
  --writeFile "generatedGraph.gv" (genDot graph)
  --quickCheck tangleAndZielonka
  quickCheckWith (sizeArg 30) spmSlidesZielonka
  quickCheck forcedPathConsistent
  quickCheck normalizeIdempotent
  quickCheck fpiFreezeZielonka
  quickCheck fpjZielonka
  quickCheck closureMonotone
  quickCheck atomicsInClosure
  quickCheck closureBounded
  quickCheck showParse
  quickCheckWith (sizeArg 12) consistentNBAChecks
  quickCheckWith (sizeArg 12) consistentNBAGNBACheck
  quickCheckWith (sizeArg 12) consistentTrimNBACheck
  quickCheckWith (sizeArg 12) consistentTrimNBACheck2
  quickCheck zielonkaConsistent
  quickCheckWith (sizeArg 25) fpiZielonka -- very slow
  where
    sizeArg n = Args (replay stdArgs) (maxSuccess stdArgs) (maxDiscardRatio stdArgs) n (chatty stdArgs) 2 -- what is this last int?

normalizeIdempotent :: LTL Int -> Bool
normalizeIdempotent x = normalize x == normalize (normalize x)

closureMonotone :: LTL Int -> Bool
closureMonotone x = all (\y -> closure y `Set.isSubsetOf` closure x) (closure x)

closureBounded :: LTL Int -> Bool
closureBounded x = foldMap closure (closure x) == closure x

atomicsInClosure :: LTL Int -> Bool
atomicsInClosure x = Set.map LTTerm (getAtomics x) `Set.isSubsetOf` closure (normalize x)

showParse :: LTL [LTLChar] -> Property
showParse x = all (not . null) (getAtomics x') ==> case parseLTL (showNoQuotes x') of
                   Left y -> counterexample (show y) False
                   Right y -> counterexample "muck" (x' == y)
      where
        x' = fmap (fmap (\(LTLChar y) -> y)) x

consistentNBAChecks :: TS Int Int Int -> LTL Int -> Bool
consistentNBAChecks x y = nbaLTLCheck x y == nbaLTLCheck2 x y

consistentNBAGNBACheck :: TS Int Int Int -> LTL Int -> Bool
consistentNBAGNBACheck x y = nbaLTLCheck x y == gnbaLTLCheck x y

consistentTrimNBACheck :: TS Int Int Int -> LTL Int -> Bool
consistentTrimNBACheck x y = nbaLTLCheck x y == reducedNBALTLCheck x y

consistentTrimNBACheck2 :: TS Int Int Int -> LTL Int -> Bool
consistentTrimNBACheck2 x y = nbaLTLCheck2 x y == reducedNBALTLCheck2 x y

zielonkaConsistent :: ParityArena -> Bool
zielonkaConsistent pa = zielonka pa == (w0,w1)
    where
      (w0,w1,_,_) = zielonkaStrat pa

tangleAndZielonka :: ParityArena -> Bool
tangleAndZielonka pa = zielonka pa == (w0,w1)
    where
      (w0,w1,_,_) = tangleLearning pa

fpiZielonka :: ParityArena -> Bool
fpiZielonka pa = zielonka pa == fpi pa

fpiFreezeZielonka :: ParityArena -> Bool
fpiFreezeZielonka pa = zielonka pa == (w0,w1)
    where
      (w0,w1,_,_)  = fpiFreeze pa

fpjZielonka :: ParityArena -> Bool
fpjZielonka pa = zielonka pa == (w0,w1)
    where
      (w0,w1,_,_) = fpj pa

spmSlidesZielonka :: ParityArena -> Bool
spmSlidesZielonka pa = zielonka pa == (w0,w1)
    where
      (w0,w1,_) = spmSlides pa Even

forcedPathConsistent :: ParityArena -> Bool
forcedPathConsistent pa = zielonka pa == (w0,w1)
    where
      (w0,w1,_,_) = forcedPathZielonka pa

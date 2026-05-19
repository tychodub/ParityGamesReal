{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where
import Test.QuickCheck
import LTL (normalize, LTL (..), closure, getAtomics, parseLTL)
import qualified Data.Set as Set
import Data.Char (isAlpha)
import Dot (showNoQuotes)
import TS
import Data.Maybe (fromJust)
import Pipeline (nbaLTLCheck, nbaLTLCheck2, gnbaLTLCheck)

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
               let sts' = Set.fromList sts
               transts <- transitions sts'
               TS sts' initsts transts <$> (stateLabels sts)
    where
      states = vector (max n 20)
      initial s = Set.fromList <$> (sublistOf s)
      transitions' s = sublistOf (Set.toList (Set.cartesianProduct s s)) 
      transitions s = Set.fromList <$> ((transitions' s) >>= (\xs -> traverse (\(l,r) -> fmap (\z -> (l,z,r)) arbitrary) xs))
      stateLabels' s =  traverse (\z -> (\y -> (z,y)) <$> arbitrary) s
      stateLabels s = fmap listToFun (stateLabels' s)
      listToFun xs a = case lookup a xs of Just x -> x; Nothing -> Set.empty

instance (Arbitrary a, Arbitrary b, Ord a, Ord b, Ord c, Arbitrary c) => Arbitrary (TS a b c) where
  arbitrary = sized sizedArb
        

main :: IO ()
main = do 
  --x <- generate (arbitrary :: Gen (TS Int Int Int))
  --print x
  quickCheck normalizeIdempotent 
  quickCheck closureMonotone
  quickCheck atomicsInClosure
  quickCheck closureBounded
  quickCheck showParse
  quickCheckWith (sizeArg 12) consistentNBAChecks
  quickCheckWith (sizeArg 12) consistentNBAGNBACheck
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

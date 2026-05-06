{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where
import Test.QuickCheck
import LTL (normalize, LTL (..), closure, getAtomics)
import qualified Data.Set as Set

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

main :: IO ()
main = do 
  quickCheck normalizeIdempotent 
  quickCheck closureMonotone
  quickCheck atomicsInClosure

normalizeIdempotent :: LTL Int -> Bool
normalizeIdempotent x = normalize x == normalize (normalize x)

closureMonotone :: LTL Int -> Bool
closureMonotone x = all (\y -> closure y `Set.isSubsetOf` closure x) (closure x)

atomicsInClosure :: LTL Int -> Bool
atomicsInClosure x = Set.map LTTerm (getAtomics x) `Set.isSubsetOf` closure (normalize x)

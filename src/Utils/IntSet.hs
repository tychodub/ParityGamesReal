module Utils.IntSet where
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.Set (Set)
import qualified Data.Set as Set

flatMap :: (Int -> IntSet) -> IntSet -> IntSet 
flatMap f s = IntSet.foldl' (\a x -> a <> f x) IntSet.empty s

flatMapS :: (a -> IntSet) -> Set a -> IntSet
flatMapS f s = Set.foldl' (\a x -> a <> f x) IntSet.empty s

toSet :: IntSet -> Set Int
toSet = Set.fromDistinctAscList . IntSet.toAscList
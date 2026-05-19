{-# LANGUAGE TypeFamilies #-}
module VWAA where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Explorer (Explorer(..))

data VWAA b a = VWAA {
    statesVWAA :: Set a,
    actsVWAA :: Set b,
    initialVWAA :: Set a,
    orderVWAA :: a -> a -> Bool,
    transitionsVWAA :: a -> b -> Set (Set (Either a a)),
    rejectVWAA :: Set a
}

instance (Show a, Show b) => Show (VWAA b a) where
    show (VWAA a t _ _ c d) = states++transitions++rejects
        where
            states = "states:\n"++foldMap (\x -> show x++"\n") a
            transitions = "transitions:\n"++foldMap (\(x,a') -> 
                foldMap (\z -> show x++" -("++show a'++")-> "++show z++"\n") (
                    foldMap (\z -> intercalate " | " (Set.toList z)) $ 
                Set.map (\x' ->
                Set.map (\z -> case z of
                Left y -> show y
                Right y -> "!"++show y) x') (c x a'))) (Set.cartesianProduct a t)
            rejects = "rejected states:\n"++foldMap (\x -> show x++"\n") d

instance (Ord a) => Explorer (VWAA b a) where
    type State (VWAA b a) = Set a
    initStates = Set.map Set.singleton . initialVWAA
    successors p s = undefined -- how to handle Either a a -- transitionsVWAA p s `Set.map` actsVWAA p

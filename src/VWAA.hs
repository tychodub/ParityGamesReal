{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module VWAA where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Explorer (Explorer(..))
import LTL

data VWAA a = VWAA {
    atomsVWAA :: Set a,
    actsVWAA :: Set (Set a),
    initialVWAA :: Set (Set a),
    -- orderVWAA :: a -> a -> Bool, -- does not seem needed anywhere in practice
    transitionsVWAA :: a -> (Set a) -> Set (Set a),
    rejectVWAA :: Set a
}


instance (Show a, Ord a) => Show (VWAA a) where
    show (VWAA a t  _ c d) = states++transitions++rejects
        where
            states = "atoms:\n"++foldMap (\x -> show x++"\n") a
            transitions = "transitions:\n"++foldMap (\(x,a') -> 
             ( show x++" -("++show a'++")-> "++transitHelper x a'
               ++"\n") ) (Set.cartesianProduct a t)
            rejects = "rejected states:\n"++foldMap (\x -> show x++"\n") d
            transitHelper x a' = intercalate " | " (fmap (intercalate " & ") (transitHelperHelper x a'))
            transitHelperHelper x a' = Set.toList $ Set.map Set.toList $ Set.map (Set.map show) (c x a')

instance (Ord a) => Explorer (VWAA a) where
    type State (VWAA a) = Set a
    initStates = initialVWAA
    successors p@(VWAA _  _ _ d _) s = Set.map (\s' -> foldMap (transit s') (actsVWAA p)) s
        where
            transit s' a = Set.unions (d s' a)

fromLTL :: Ord prop => LTL prop -> VWAA (LTL prop)
fromLTL ltl = VWAA states (Set.powerSet closure') initialStates transit Set.empty
    where
        closure' = closure (normalize $ simplifyLtl ltl)
        states = Set.filter proper closure'
        initialStates = Set.singleton $ Set.singleton ltl
        proper LTTrue = False
        proper LTFalse = False
        proper (LTOr _ _) = False
        proper (LTAnd _ _) = False
        proper _ = True
        setTensor l r = Set.map (uncurry Set.union) $ Set.cartesianProduct l r
        transit (LTX x) _ = Set.singleton (Set.singleton x)
        transit LTTrue _ = Set.singleton Set.empty
        transit LTFalse _ = Set.empty
        transit ltl@(LTTerm x) a | ltl `Set.member` a = Set.singleton Set.empty
                                 | otherwise = Set.empty
        transit ltl@(LTNot x) a | not (ltl `Set.member` a) = Set.singleton Set.empty
                                | otherwise = Set.empty
        transit (LTOr l r) a = transit l a <> transit r a
        transit (LTAnd l r) a = setTensor (transit l a) (transit r a)
        transit (LTU l r) a = transit r a <> setTensor (transit l a) (Set.singleton $ Set.singleton (LTU l r))
        transit (LTR l r) a = (transit r a) `setTensor` (transit l a <> (Set.singleton $ Set.singleton (LTR l r)))
        transit _ _ = error "unsupported state for transition relation"

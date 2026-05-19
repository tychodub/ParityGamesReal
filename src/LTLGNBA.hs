module LTLGNBA where
import LTL (LTL (..), normalize, getAtomics, closure, consistentSubsetsLTL, elemLTL, simplifyLtl)
import GNBA
import Data.Set (Set)
import qualified Data.Set as Set

fromLTL :: Ord prop => LTL prop -> GNBA (Set (LTL prop)) (Set prop)
fromLTL ltl = GNBA states initialStates transitions finalStates
    where
        atoms = Set.map LTTerm $ getAtomics ltl
        normalizedLTL = normalize (simplifyLtl ltl)
        closureOfLTL = closure normalizedLTL
        states = consistentSubsetsLTL closureOfLTL
        initialStates = Set.filter (\x -> normalizedLTL `Set.member` x) states
        transitions = foldMap (\s -> let sigma = Set.intersection atoms s in
            Set.map (\x -> (s,Set.map (\(LTTerm name) -> name) sigma,x)) 
            $ Set.filter (transitionAllowed closureOfLTL s) states) states
        finalStates = Set.foldl' (\s x -> case x of 
                    (LTU a b) -> flip Set.insert s $ Set.filter (\s' -> not (LTU a b `Set.member` s') || b `elemLTL` s') states
                    _ -> s) Set.empty closureOfLTL


transitionAllowed :: Ord prop => Set (LTL prop) -> Set (LTL prop)-> Set (LTL prop) -> Bool
transitionAllowed closureOfLTL l r = all subCheck closureOfLTL
    where
        subCheck (LTX a) = (LTX a `Set.member` l) == a `elemLTL` r
        subCheck (LTU a b) = (LTU a b `Set.member` l) == (b `elemLTL` l || (a `elemLTL` l && LTU a b `Set.member` r))
        subCheck (LTR a b) = (LTR a b `Set.member` l) == (b `elemLTL` l && (a `elemLTL` l || LTR a b `Set.member` r))
        subCheck _ = True

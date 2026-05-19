module Pipeline where
import LTL
import TS
import LTLGNBA (fromLTL)
import NBA (nbaFromGnba, tsMul, dfsAcceptingLasso, NBA (initialNBA, acceptingNBA))
import qualified GNBA
import Explorer (tarjanNontrivial)
import qualified Data.Set as Set
import Data.Map (elems)

nbaLTLCheck :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
nbaLTLCheck x y = not $ dfsAcceptingLasso prod
    where
        negy = LTNot y
        atomics = getAtomics negy
        gnba = fromLTL negy
        nba  = nbaFromGnba gnba
        prod = tsMul x nba atomics

nbaLTLCheck2 :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
nbaLTLCheck2 x y = not $ any (\parts -> any (\part -> any (\x' -> x' `Set.member` (acceptingNBA prod)) part) (elems parts)) tarj
    where
        negy = LTNot y
        atomics = getAtomics negy
        gnba = fromLTL negy
        nba  = nbaFromGnba gnba
        prod = tsMul x nba atomics
        tarj = Set.map (tarjanNontrivial prod) (initialNBA prod)

gnbaLTLCheck :: (Ord prop, Ord a, Ord b, Show a, Show prop) => TS a b prop -> LTL prop -> Bool
gnbaLTLCheck x y = not $ GNBA.gnbaAccepting prod
    where
        negy = LTNot y
        atomics = getAtomics negy
        gnba = fromLTL negy
        prod = GNBA.tsMul x gnba atomics

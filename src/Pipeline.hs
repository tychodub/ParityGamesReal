module Pipeline where
import LTL
import TS
import LTLGNBA (fromLTL)
import NBA (nbaFromGnba, tsMul, dfsAcceptingLasso, NBA (initialNBA, acceptingNBA), trimNBA)
import qualified GNBA
import Explorer (tarjanNontrivial)
import qualified Data.Set as Set
import Data.Map (elems)
import PetriNet (Petri)
import LTLXML (Fireable, Cardinality)

mccNBACheck :: Petri String String -> LTL (Either Fireable (Cardinality String)) -> Bool
mccNBACheck x y = nbaLTLCheck ts y
    where
        ts = fromPetri x (getAtomics y) 

mccNBACheck2 :: Petri String String -> LTL (Either Fireable (Cardinality String)) -> Bool
mccNBACheck2 x y = nbaLTLCheck2 ts y
    where
        ts = fromPetri x (getAtomics y) 

mccGNBACheck :: Petri String String -> LTL (Either Fireable (Cardinality String)) -> Bool
mccGNBACheck x y = gnbaLTLCheck ts y
    where
        ts = fromPetri x (getAtomics y) 

mccReducedNBACheck :: Petri String String -> LTL (Either Fireable (Cardinality String)) -> Bool
mccReducedNBACheck x y = reducedNBALTLCheck ts y
    where
        ts = fromPetri x (getAtomics y) 

mccReducedNBACheck2 :: Petri String String -> LTL (Either Fireable (Cardinality String)) -> Bool
mccReducedNBACheck2 x y = reducedNBALTLCheck2 ts y
    where
        ts = fromPetri x (getAtomics y) 

nbaLTLCheck :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
nbaLTLCheck x y = not $ dfsAcceptingLasso prod
    where
        negy = LTNot y
        gnba = fromLTL negy
        nba  = nbaFromGnba gnba
        prod = tsMul x nba

nbaLTLCheck2 :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
nbaLTLCheck2 x y = not $ any (\parts -> any (\part -> any (\x' -> x' `Set.member` (acceptingNBA prod)) part) (elems parts)) tarj
    where
        negy = LTNot y
        gnba = fromLTL negy
        nba  = nbaFromGnba gnba
        prod = tsMul x nba
        tarj = Set.map (tarjanNontrivial prod) (initialNBA prod)

gnbaLTLCheck :: (Ord prop, Ord a, Ord b, Show a, Show prop) => TS a b prop -> LTL prop -> Bool
gnbaLTLCheck x y = not $ GNBA.gnbaAccepting prod
    where
        negy = LTNot y
        gnba = fromLTL negy
        prod = GNBA.tsMul x gnba

reducedNBALTLCheck :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
reducedNBALTLCheck x y = not $ dfsAcceptingLasso prod
    where
        negy = LTNot y
        gnba = fromLTL negy
        nba' = nbaFromGnba gnba
        nba = trimNBA nba'
        prod' = tsMul x nba
        prod = trimNBA prod'

reducedNBALTLCheck2 :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
reducedNBALTLCheck2 x y = not $ any (\parts -> any (\part -> any (\x' -> x' `Set.member` (acceptingNBA prod)) part) (elems parts)) tarj
    where
        negy = LTNot y
        gnba = fromLTL negy
        nba' = nbaFromGnba gnba
        nba = trimNBA nba'
        prod = tsMul x nba
        tarj = Set.map (tarjanNontrivial prod) (initialNBA prod)

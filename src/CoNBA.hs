{-# LANGUAGE TypeFamilies #-}
module CoNBA where

import Data.Set (Set)
import Explorer
import qualified Data.Set as Set
import Dot (Dot(..), showNoQuotes)
import CoGNBA (CoGNBA (CoGNBA))
import qualified Data.Sequence as Seq

data CoNBA a b = CoNBA { 
    statesCoNBA :: Set a,
    initialCoNBA :: Set a,
    transitionsCoNBA :: Set (a,b,a),
    rejectingCoNBA :: Set a
} deriving Eq

instance (Eq a, Show a, Show b) => Show (CoNBA a b) where
    show nba = "states: "++concatMap (\x -> show x++i x++", ") (statesCoNBA nba)++"\ntransitions:\n"
                ++concatMap (\x -> show x++",\n") ((transitionsCoNBA nba))++"accepting: "
                ++show (Set.toList (rejectingCoNBA nba))
        where
            i x = if x `elem` (initialCoNBA nba) then " (i)" else ""

instance (Ord a) => Explorer (CoNBA a b) where
    type State (CoNBA a _) = a
    initStates = initialCoNBA
    successors nba s = Set.map (\(_,_,y) -> y) $ Set.filter (\(x1,_,_) -> x1 == s) (transitionsCoNBA nba)

instance (Show a, Show b, Ord a) => Dot (CoNBA a b) where
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\""++ifAccept y++ifInit y) (statesCoNBA x)
        where
            ifAccept y = if y `Set.member` rejectingCoNBA x then "[shape = doublecircle]" else ""
            ifInit y = if y `Set.member` (initialCoNBA x)
                then "[color = \"green\"]"
                else ""
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (transitionsCoNBA x)
    dotName _ = "nba"

-- | creates a n-fold disjoint union where n is the amount of rejecting sets
coNbaFromCoGnba :: (Ord a, Ord b) => CoGNBA a b -> CoNBA (a, Int) b
coNbaFromCoGnba (CoGNBA a b c d) = CoNBA coNbaStates coNbaInit coNbaTransitions coNbaReject
    where
        n = length d
        nSet = Set.fromList [0..n-1]
        coNbaStates = Set.cartesianProduct a nSet
        coNbaInit = Set.cartesianProduct b nSet
        coNbaTransitions = foldMap (\i -> Set.map (\(l,sigma,r) -> ((l,i),sigma,(r,i))) c) nSet
        coNbaReject = Seq.foldMapWithIndex (\i s -> Set.map (\x -> (x,i)) s) (Seq.fromList $ Set.toList d)

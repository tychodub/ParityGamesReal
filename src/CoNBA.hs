{-# LANGUAGE TypeFamilies #-}
module CoNBA where

import Data.Set (Set)
import Explorer
import qualified Data.Set as Set
import Dot (Dot(..), showNoQuotes)
import CoGNBA (CoGNBA (CoGNBA))
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)

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
    dotName _ = "conba"

nbaFromGnba :: (Ord a, Ord b) => CoGNBA a b -> CoNBA (a, Int) b
nbaFromGnba (CoGNBA a b c d) = CoNBA nbaStates nbaInit nbaTransitions nbaAccept
    where
        n = length d
        finalsList = Seq.fromList $ Set.toList d
        nbaInit = Set.map (\x -> (x, 0)) b
        nbaAccept = if n > 0 then Set.map (\x -> (x,0)) $ fromJust (finalsList Seq.!? 0) else Set.empty
        nbaStates = foldMap (\x -> Set.fromList ((,) x <$> [0..n-1])) a
        nbaTransitions = foldMap (\(x,m) -> if x `Set.member` fromJust (finalsList Seq.!? m) 
            then Set.map (\(l,act,r) -> ((l,m),act,(r,m+1 `rem` n))) $ Set.filter (\(l,_,_) -> l == x) c 
            else Set.map (\(l,act,r) -> ((l,m),act,(r,m))) $ Set.filter (\(l,_,_) -> l == x) c ) nbaStates

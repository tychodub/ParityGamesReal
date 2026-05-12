{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module GNBA where
import Data.Set (Set, toList)
import Explorer (Explorer(..), tarjanNontrivial)
import qualified Data.Set as Set
import Dot (Dot(..), showNoQuotes)
import qualified Data.Sequence as Seq
import TS
import qualified Data.Map as Map
import Debug.Trace

data GNBA a b = GNBA { 
    statesGNBA :: Set a,
    initialGNBA :: Set a,
    transitionsGNBA :: Set (a,b,a),
    acceptingGNBA :: Set (Set a)
} deriving Eq

instance (Eq a, Show a, Show b) => Show (GNBA a b) where
    show gnba = "states: "++concatMap (\x -> show x++i x++", ") (statesGNBA gnba)++"\ntransitions:\n"
                ++concatMap (\x -> show x++",\n") ((transitionsGNBA gnba))++"accepting: "
                ++show (toList (acceptingGNBA gnba))
        where
            i x = if x `elem` (initialGNBA gnba) then " (i)" else ""

instance (Ord a) => Explorer (GNBA a b) where
    type State (GNBA a _) = a
    initStates = initialGNBA
    successors gnba s = Set.map (\(_,_,y) -> y) $ Set.filter (\(x1,_,_) -> x1 == s) (transitionsGNBA gnba)

instance (Show a, Ord a, Show b) => Dot (GNBA a b) where
    dotNodes :: (Show a, Show b) => GNBA a b -> Set String
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\""++ifAccept y++ifInit y) (statesGNBA x)
        where
            acceptIds = Seq.fromList $ toList $ acceptingGNBA x
            acceptText y = "\n" ++ Seq.foldMapWithIndex (\n z -> 
                                if y `Set.member` z then show n++" " else "") acceptIds 
            ifAccept y = if any (y `Set.member`) (acceptingGNBA x) 
                then " [shape = doublecircle] [label = \""++showNoQuotes y++acceptText y++"\"]" 
                else ""
            ifInit y = if y `Set.member` (initialGNBA x)
                then "[color = \"green\"]"
                else ""
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (transitionsGNBA x)
    dotName _ = "gnba"
    dotpreamble _ = "    node [colorscheme = spectral11];\n"

gnbaBimap :: (Ord c, Ord d) => (a -> c) -> (b -> d) -> GNBA a b -> GNBA c d
gnbaBimap f g (GNBA a b c d) = GNBA (Set.map f a) 
                                    (Set.map f b) 
                                    (Set.map (\(x,y,z) -> (f x, g y, f z)) c) 
                                    (Set.map (Set.map f) d)

-- currently each accepting set {F1,F2,F3} becomes an accepting s `cartesianProduct` {F1,F2,F3}
tsMul :: (Ord a, Ord s, Ord c, Ord b) => TS a b c -> GNBA s (Set c) -> GNBA (a,s) b 
tsMul x y = GNBA states newInitial transitions finalStates
    where
        states = Set.cartesianProduct (tsStates x) (statesGNBA y)
        finalStates = foldMap (Set.map (\f -> Set.map (\z -> (z,f)) (tsStates x))) (acceptingGNBA y) 
        rightCond s = Set.filter (\(_,z,_) -> z == tsLabels x s) (transitionsGNBA y) 
        combineTransitions (l,a,r) (p,_,q) = ((l,p),a,(r,q))
        -- transitions are correct
        transitions = foldMap (\(l,a,r) -> Set.map (combineTransitions (l,a,r)) (rightCond r)) (tsTransitions x)
        initialUnfiltered = Set.cartesianProduct (tsInitial x) (statesGNBA y)
        newInitial = Set.filter (\(s,q) -> 
            any (\q' -> (q',tsLabels x s,q) `Set.member` (transitionsGNBA y)) 
                    (initialGNBA y)) 
                    initialUnfiltered

gnbaAccepting :: Ord a => GNBA a b -> Bool
gnbaAccepting x = not (null (acceptingGNBA x)) && any sccCheck tarj
    where
        tarj = Set.map (tarjanNontrivial x) (initialGNBA x)
        sccCheck parts = any (\part -> all (any (\f -> Set.member f part)) (acceptingGNBA x)) 
                             (Map.elems parts)

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

data GNBA a b = GNBA { 
    statesGNBA :: [a],
    initialGNBA :: Set a,
    alphabetGNBA :: Set b,
    transitionsGNBA :: a -> b -> [a],
    acceptingGNBA :: Set (Set a)
}

instance (Eq a, Eq b, Ord a) => Eq (GNBA a b) where
    l == r = (Set.fromList $ statesGNBA l) == (Set.fromList $ statesGNBA r) && initialGNBA l == initialGNBA r && 
             alphabetGNBA l == alphabetGNBA r &&
             Set.fromList (transitionsGNBA l<$>statesGNBA l<*>Set.toList (alphabetGNBA l)) == 
                Set.fromList (transitionsGNBA r<$>statesGNBA r<*>Set.toList (alphabetGNBA r)) &&
             acceptingGNBA l == acceptingGNBA r

instance (Show a, Show b, Ord b, Ord a) => Show (GNBA a b) where
    show gnba = "states: "++concatMap (\x -> show x++i x++", ") (statesGNBA gnba)++"\ntransitions:\n"
                ++concatMap (\x -> show x++",\n") ((finTransitionsGNBA gnba))++"accepting: "
                ++show (toList (acceptingGNBA gnba))
        where
            i x = if x `elem` (initialGNBA gnba) then " (i)" else ""

instance (Ord a) => Explorer (GNBA a b) where
    type State (GNBA a _) = a
    initStates = initialGNBA
    successors gnba s = foldMap (Set.fromList . transitionsGNBA gnba s) (alphabetGNBA gnba)

instance (Show a, Ord a, Show b, Ord b) => Dot (GNBA a b) where
    dotNodes :: (Show a, Show b) => GNBA a b -> Set String
    dotNodes x = Set.fromList $ map (\y -> "\""++showNoQuotes y++"\""++ifAccept y++ifInit y) (statesGNBA x)
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
                                        "\""++showNoQuotes c++"\"")) (finTransitionsGNBA x)
    dotName _ = "gnba"
    dotpreamble _ = "    node [colorscheme = spectral11];\n"

finTransitionsGNBA :: (Ord b, Ord a) => GNBA a b -> Set (a,b,a)
finTransitionsGNBA (GNBA a _ c d _) = Set.fromList ([(x,y,z) | x <- a, y <- Set.toList c, z <- d x y])

-- currently each accepting set {F1,F2,F3} becomes an accepting s `cartesianProduct` {F1,F2,F3}
tsMul :: (Ord a, Ord s, Ord c, Ord b) => TS a b c -> GNBA s (Set c) -> GNBA (a,s) b 
tsMul x y = GNBA states newInitial (tsAlphabet x) transitions finalStates
    where
        states = [(l,r) | l <- (tsStates x), r <- (statesGNBA y)]
        finalStates = Set.map (foldMap (\f -> Set.map (\z -> (z,f)) (Set.fromList $ tsStates x))) (acceptingGNBA y) 
        transitions (l,r) act = foldMap (\l' -> map (\r' -> (l',r')) $ transitionsGNBA y r (tsLabels x l')) 
                               $ tsTransitions x l act
        newInitial = foldMap (\(l,r) -> Set.map (\r' -> (l,r')) r) $ Set.map (\(s,q') -> (s,
            (Set.fromList . transitionsGNBA y q') (tsLabels x s))) 
            (Set.cartesianProduct (tsInitial x) (initialGNBA y))

gnbaAccepting :: Ord a => GNBA a b -> Bool
gnbaAccepting x = any sccCheck tarj 
    where
        tarj = Set.map (tarjanNontrivial x) (initialGNBA x)
        sccCheck parts = any (\part -> all (\accs -> any (\x' -> x' `Set.member` accs) part) (acceptingGNBA x)) 
                             (Map.elems parts)

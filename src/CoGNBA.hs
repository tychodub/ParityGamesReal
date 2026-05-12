{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module CoGNBA where
import Data.Set (Set, toList)
import Explorer (Explorer(..))
import qualified Data.Set as Set
import Dot (Dot(..), showNoQuotes)
import qualified Data.Sequence as Seq

data CoGNBA a b = CoGNBA { 
    statesCoGNBA :: Set a,
    initialCoGNBA :: Set a,
    transitionsCoGNBA :: Set (a,b,a),
    rejectingCoGNBA :: Set (Set a)
} deriving Eq

instance (Eq a, Show a, Show b) => Show (CoGNBA a b) where
    show gnba = "states: "++concatMap (\x -> show x++i x++", ") (statesCoGNBA gnba)++"\ntransitions:\n"
                ++concatMap (\x -> show x++",\n") ((transitionsCoGNBA gnba))++"rejecting: "
                ++show (toList (rejectingCoGNBA gnba))
        where
            i x = if x `elem` (initialCoGNBA gnba) then " (i)" else ""

instance (Ord a) => Explorer (CoGNBA a b) where
    type State (CoGNBA a _) = a
    initStates = initialCoGNBA
    successors gnba s = Set.map (\(_,_,y) -> y) $ Set.filter (\(x1,_,_) -> x1 == s) (transitionsCoGNBA gnba)

instance (Show a, Ord a, Show b) => Dot (CoGNBA a b) where
    dotNodes :: (Show a, Show b) => CoGNBA a b -> Set String
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\""++ifAccept y++ifInit y) (statesCoGNBA x)
        where
            acceptIds = Seq.fromList $ toList $ rejectingCoGNBA x
            acceptText y = "\n" ++ Seq.foldMapWithIndex (\n z -> 
                                if y `Set.member` z then show n++" " else "") acceptIds 
            ifAccept y = if any (y `Set.member`) (rejectingCoGNBA x) 
                then " [shape = doublecircle] [label = \""++showNoQuotes y++acceptText y++"\"]" 
                else ""
            ifInit y = if y `Set.member` (initialCoGNBA x)
                then "[color = \"green\"]"
                else ""
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (transitionsCoGNBA x)
    dotName _ = "cognba"
    dotpreamble _ = "    node [colorscheme = spectral11];\n"

{-# LANGUAGE TypeFamilies #-}
module TS where
import Data.Set (Set, toList)
import Data.List (intercalate)
import qualified Data.Set as Set
import Dot (Dot(..), showNoQuotes)
import Explorer (Explorer(..))

data TS a b c = TS {
    tsStates :: Set a,
    tsInitial :: Set a,
    tsTransitions :: Set (a,b,a),
    tsLabels :: a -> Set c
}

instance (Show a, Show b, Show c) => Show (TS a b c) where
    show x = "states:\n"++foldMap 
                (\y -> (show y)++" ("++intercalate ", " (map show $ toList $ tsLabels x y)++")\n") (tsStates x)++
             "\ninitial:\n"++foldMap ((++"\n") . show) (tsInitial x)++
             "\ntransitions:\n"++foldMap ((++"\n") . show) (tsTransitions x)

instance (Eq a, Eq b, Eq c, Ord c) => Eq (TS a b c) where
    l == r = tsStates l == tsStates r && tsInitial l == tsInitial r
            && tsTransitions l == tsTransitions r && Set.map (tsLabels l) (tsStates l) == Set.map (tsLabels r) (tsStates r)

instance (Show a, Show b, Show c) => Dot (TS a b c) where
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\" [label = "++"\""++showNoQuotes y++dotLabels y++"\""++"]") (tsStates x)
        where
            dotLabels y = foldMap (\z -> "\n"++showNoQuotes z) (tsLabels x y)
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (tsTransitions x)
    dotName _ = "TS"

instance Ord a => Explorer (TS a b c) where
    type State (TS a _ _) = a
    initStates t = tsInitial t
    successors t s = Set.map (\(_,_,r) -> r) $ Set.filter (\(l,_,_) -> l==s) (tsTransitions t)

completeTS :: Ord a => Set a -> Set a -> (a -> Set c) -> TS a () c
completeTS a b c = TS a b transitions c 
    where
        transitions = Set.map (\(l,r) -> (l,(),r)) $ Set.cartesianProduct a a

discreteTS :: Ord a => Set a -> Set a -> (a -> Set c) -> TS a () c
discreteTS a b c = TS a b Set.empty c

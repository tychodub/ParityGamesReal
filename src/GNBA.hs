{-# LANGUAGE TypeFamilies #-}
module GNBA where
import Data.Set (Set, toList)
import Explorer (Explorer(..))
import qualified Data.Set as Set

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

gnbaBimap :: (Ord c, Ord d) => (a -> c) -> (b -> d) -> GNBA a b -> GNBA c d
gnbaBimap f g (GNBA a b c d) = GNBA (Set.map f a) 
                                    (Set.map f b) 
                                    (Set.map (\(x,y,z) -> (f x, g y, f z)) c) 
                                    (Set.map (Set.map f) d)

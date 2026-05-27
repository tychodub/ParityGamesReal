{-# LANGUAGE TypeFamilies #-}
module TGBA where
import Data.Set (Set)
import Data.Foldable (Foldable(..))
import Explorer (Explorer(..))
import qualified Data.Set as Set


data TGBA a = TGBA { 
    statesTGBA :: Set a,
    initialTGBA :: Set (Set a),
    transitionsTGBA :: a -> Set a -> Set (Set a),
    acceptingTGBA :: Set (Set a)
}

{-
instance (Eq a, Show a) => Show (TGBA a) where
    show gnba = "states: "++concatMap (\x -> show x++i x++", ") (statesTGBA gnba)++"\ntransitions:\n"
                ++concatMap (\x -> show x++",\n") ((transitionsTGBA gnba))++"accepting: "
                ++show (toList (acceptingTGBA gnba))
        where
            i x = if any (x `elem`) (initialTGBA gnba) then " (i)" else ""
-}
instance (Ord a) => Explorer (TGBA a) where
    type State (TGBA a) = Set a
    initStates = initialTGBA
    successors p s = Set.map (undefined) s

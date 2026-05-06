{-# LANGUAGE TypeFamilies #-}
module TS where
import Data.Set (Set, toList)
import Data.List (intercalate)
import qualified Data.Set as Set

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

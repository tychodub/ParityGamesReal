{-# LANGUAGE TypeFamilies #-}
module NBA where
import Data.Set (Set)
import Explorer
import qualified Data.Set as Set
import GNBA (GNBA (..))
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)
import Dot (Dot(..), showNoQuotes)

data NBA a b = NBA { 
    statesNBA :: Set a,
    initialNBA :: Set a,
    transitionsNBA :: Set (a,b,a),
    acceptingNBA :: Set a
} deriving Eq

instance (Eq a, Show a, Show b) => Show (NBA a b) where
    show nba = "states: "++concatMap (\x -> show x++i x++", ") (statesNBA nba)++"\ntransitions:\n"
                ++concatMap (\x -> show x++",\n") ((transitionsNBA nba))++"accepting: "
                ++show (Set.toList (acceptingNBA nba))
        where
            i x = if x `elem` (initialNBA nba) then " (i)" else ""

instance (Ord a) => Explorer (NBA a b) where
    type State (NBA a _) = a
    initStates = initialNBA
    successors nba s = Set.map (\(_,_,y) -> y) $ Set.filter (\(x1,_,_) -> x1 == s) (transitionsNBA nba)

instance (Show a, Show b, Ord a) => Dot (NBA a b) where
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\""++ifAccept y) (statesNBA x)
        where
            ifAccept y = if y `Set.member` acceptingNBA x then "[shape = doublecircle]" else ""
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (transitionsNBA x)
    dotName _ = "nba"

-- could theoretically be done without Ord b, but not worth the time
nbaFromGnba :: (Ord a, Ord b) => GNBA a b -> NBA (a, Int) b
nbaFromGnba (GNBA a b c d) = NBA nbaStates nbaInit nbaTransitions nbaAccept
    where
        n = length d
        finalsList = Seq.fromList $ Set.toList d
        nbaInit = Set.map (\x -> (x, 0)) b
        nbaAccept = if n > 0 then Set.map (\x -> (x,0)) $ fromJust (finalsList Seq.!? 0) else Set.empty
        nbaStates = foldMap (\x -> Set.fromList ((,) x <$> [0..n-1])) a
        nbaTransitions = foldMap (\(x,m) -> if x `Set.member` fromJust (finalsList Seq.!? m) 
            then Set.map (\(l,act,r) -> ((l,m),act,(r,m+1 `rem` n))) $ Set.filter (\(l,_,_) -> l == x) c 
            else Set.map (\(l,act,r) -> ((l,m),act,(r,m))) $ Set.filter (\(l,_,_) -> l == x) c ) nbaStates

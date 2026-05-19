{-# LANGUAGE TypeFamilies #-}
module NBA where
import Data.Set (Set)
import Explorer
import qualified Data.Set as Set
import GNBA (GNBA (..))
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust, isNothing)
import Dot (Dot(..), showNoQuotes)
import TS (TS (..))
import Debug.Trace (trace)

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
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\""++ifAccept y++ifInit y) (statesNBA x)
        where
            ifAccept y = if y `Set.member` acceptingNBA x then "[shape = doublecircle]" else ""
            ifInit y = if y `Set.member` (initialNBA x)
                then "[color = \"green\"]"
                else ""
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (transitionsNBA x)
    dotName _ = "nba"

-- could theoretically be done without Ord b, but not worth the efficiency loss/time
nbaFromGnba :: (Ord a, Ord b) => GNBA a b -> NBA (a, Int) b
nbaFromGnba (GNBA a b c d) = NBA nbaStates nbaInit nbaTransitions nbaAccept
    where
        n = length d
        finalsList = if n > 0 then Seq.fromList $ Set.toList d else Seq.singleton a
        nbaInit = Set.map (\x -> (x, 0)) b
        nbaAccept = Set.map (\x -> (x,0)) $ fromJust (finalsList Seq.!? 0)
        nbaStates = Set.cartesianProduct a (Set.fromList [0..max (n-1) 0])
        nbaTransitions = foldMap (\(x,m) -> if x `Set.member` fromJust (finalsList Seq.!? m) 
            then Set.map (\(l,act,r) -> ((l,m),act,(r,(m+1) `rem` (max n 1)))) $ Set.filter (\(l,_,_) -> l == x) c 
            else Set.map (\(l,act,r) -> ((l,m),act,(r,m))) $ Set.filter (\(l,_,_) -> l == x) c ) nbaStates

tsMul :: (Ord a, Ord s, Ord c, Ord b) => TS a b c -> NBA s (Set c) -> Set c -> NBA (a,s) b 
tsMul x y atomics = NBA states newInitial transitions finalStates
    where
        states = Set.cartesianProduct (tsStates x) (statesNBA y)
        finalStates = Set.cartesianProduct (tsStates x) (acceptingNBA y) 
        rightCond s = Set.filter (\(_,z,_) -> z == Set.intersection atomics (tsLabels x s)) (transitionsNBA y) 
        combineTransitions (l,a,r) (p,_,q) = ((l,p),a,(r,q))
        transitions = foldMap (\(l,a,r) -> Set.map (combineTransitions (l,a,r)) (rightCond r)) (tsTransitions x)
        initialUnfiltered = Set.cartesianProduct (tsInitial x) (statesNBA y)
        newInitial = Set.filter (\(s,q) -> 
            any (\q' -> (q',tsLabels x s,q) `Set.member` (transitionsNBA y)) 
                    (initialNBA y)) 
                    initialUnfiltered

data Colour = Cyan | Blue | Red | White deriving (Show, Eq, Ord)

updateColour :: Eq t => (t -> p) -> t -> p -> t -> p
updateColour f s c = (\x -> if x == s then c else f x)

dfsAcceptingLasso :: (Ord a) => NBA a b -> Bool
dfsAcceptingLasso p = any (dfsLasso p) foundAccepting
    where
        foundAccepting = Set.filter (`Set.member` (acceptingNBA p)) (explore p)

dfsLasso :: (Ord a) => NBA a b -> a -> Bool 
dfsLasso p s = isNothing $ dfsBlue p s (\_ -> White)

dfsBlue :: (Ord a) => NBA a b -> a -> (a -> Colour) -> Maybe (a -> Colour)
dfsBlue p s f = if s `Set.member` (acceptingNBA p) 
                   then fmap (\h -> updateColour h s Red) (foldSuccs >>= (dfsRed p s))
                   else fmap (\h -> updateColour h s Blue) foldSuccs
    where
      f' = updateColour f s Cyan
      foldSuccs = Set.foldl' (\g s' -> case g of (Just g') -> if g' s' == White then dfsBlue p s' g' else pure g'
                                                 Nothing -> Nothing) (Just f') (successors p s)

dfsRed :: (Ord a) => NBA a b -> a -> (a -> Colour) -> Maybe (a -> Colour)
dfsRed p s f' = dfsRedHelper (Set.toList $ successors p s) f'
    where
        dfsRedHelper [] f = pure f 
        dfsRedHelper (t:ts) f | f t == Cyan = Nothing
                              | f t == Blue = dfsRed p t (updateColour f t Red) >>= dfsRedHelper ts 
                              | otherwise   = dfsRedHelper ts f

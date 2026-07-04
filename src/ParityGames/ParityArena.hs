{-# LANGUAGE TypeFamilies #-}
module ParityGames.ParityArena where
import Data.Graph (Graph, vertices, edges, graphFromEdges)
import Explorer (Explorer(..))
import qualified Data.Set as Set
import Dot
import Data.Set (Set)
import qualified GHC.Arr as Array
import Data.Maybe (fromJust)
import qualified Data.Graph as Graph
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

class ParityClass a where
    verticesPA :: a -> [Int]
    priorityPA :: a -> Int -> Int
    ownsPA :: a -> Int -> Bool
    successorsPA :: a -> Int -> Set Int

data ParityGame a = ArenaPA { 
    forgetPA :: Graph, 
    prioPA :: Int -> Int,
    evenOwns :: Int -> Bool,
    indexToNode :: Int -> a 
}

instance ParityClass (ParityGame a) where
    verticesPA = vertices . forgetPA
    priorityPA = prioPA
    ownsPA = evenOwns
    successorsPA = successors

type ParityArena = ParityGame Int

flatPA :: Graph -> ParityArena
flatPA graph = ArenaPA graph id even id

flatParityFromEdges :: Ord a => [(a, [a])] -> ParityGame a
flatParityFromEdges es = ArenaPA a id even newToNode
    where
        (a,b,_) = graphFromEdges (map (\(l,r) -> (l,l,r)) es)
        newToNode = \n -> let (_,y,_) = b n in y

-- | this function removes nodes with no predecessor
--  warning: this reindexes the nodes of the graph
pruneLeafs :: ParityGame a -> (ParityGame a, Int -> Maybe Int, Int -> Maybe Int)
pruneLeafs pa@(ArenaPA graph _ _ _) = (newGraph, toOG, c)
    where
        leafs = Set.filter (\x -> null (invertedG Array.! x)) $ vsSet
        vsSet = Set.fromList (vertices graph)
        toRemove leafs' leftover | null newLeafs = leafs'
                                 | otherwise = toRemove (leafs' <> newLeafs) (leftover Set.\\ newLeafs)
            where
                newLeafs = Set.filter (\x -> all (`elem` leafs') (invertedG Array.! x)) leftover
        invertedG = Graph.transposeG graph
        finalLeafs = toRemove leafs (vsSet Set.\\ leafs)
        leftoverVS = vsSet Set.\\ finalLeafs
        (newGraph,toOG,c) = subGame pa leftoverVS

instance Show (ParityGame a) where
    show = show . forgetPA

instance Show a => Dot (ParityGame a) where
  dotNodes (ArenaPA graph pri evenOwn toNode) = Set.fromList $ map 
      (\x ->"\""++showNoQuotes (toNode x)++"\" [label = \""++show (toNode x)++", "++show (pri x)++"\"]"++ifEvenOwns x) (vertices graph)
      where
        ifEvenOwns x = if evenOwn x then "" else " [shape = pentagon]"
  dotArrows pa = Set.fromList (map (\(l,r) -> (showNQWithQ (indexToNode pa l),"\"\"",showNQWithQ (indexToNode pa r))) $ edges (forgetPA pa))
      where
        showNQWithQ x = "\""++(showNoQuotes x)++"\""
  dotName _ = "ParityArena"

instance Explorer (ParityGame a) where
    type State (ParityGame _) = Int
    initStates pa = Set.fromList $ vertices $ forgetPA pa
    successors pa n = if n `elem` (vertices (forgetPA pa)) then Set.fromList ((forgetPA pa) Array.! n)
                            else error ("subGame got incorrect index, n: "++show n++"\ngraph: "++show (forgetPA pa))

instance Functor ParityGame where
    fmap f (ArenaPA graph pri evenOwn toNode) = ArenaPA graph pri evenOwn (f . toNode)

data Player = Even | Odd deriving (Show, Eq, Ord, Enum)

attractors :: ParityGame a -> IntSet -> Player -> IntSet
attractors pa@(ArenaPA graph _ owns _) xs Even | xs == newXs = xs
                                               | otherwise = attractors pa newXs Even
    where
        newXs = xs <> canPlay <> mustPlay
        canPlay = foldl' (\s (l,r) -> if r `IntSet.member` xs && owns l then IntSet.insert l s else s) IntSet.empty (edges graph)
        mustPlay = IntSet.fromList $ filter (\x -> all (`IntSet.member` xs) (successors pa x)) (vertices graph)
attractors pa@(ArenaPA graph _ owns _) xs Odd | xs == newXs = xs
                                              | otherwise = attractors pa newXs Odd
    where
        newXs = xs <> canPlay <> mustPlay
        canPlay = foldl' (\s (l,r) -> if r `IntSet.member` xs && not (owns l) then IntSet.insert l s else s) IntSet.empty (edges graph)
        mustPlay = IntSet.fromList $ filter (\x -> all (`IntSet.member` xs) (successors pa x)) (vertices graph)

-- not yet fully deprecated, but better to move over to SubGamePa interface
subGame :: Foldable t => ParityGame a -> t Int -> (ParityGame a, Int -> Maybe Int, Int -> Maybe Int)
subGame (ArenaPA graph pri owns tn) s = (ArenaPA a newPri newOwns newToIndex, toOG, c) 
    where
        vs = filter (\v -> elem v s) (vertices graph)
        es = map (\n ->  (n,n,graph Array.! n)) vs
        (a,b,c) = Data.Graph.graphFromEdges es
        newPri n = let (x,_,_) = b n in pri x
        newOwns n = let (x,_,_) = b n in owns x
        newToIndex n = let (x,_,_) = b n in tn x
        toOG x = if x `elem` vertices a then let (y,_,_) = b x in Just y else Nothing

-- | deprecated, just here for backwards compatibility
{-# DEPRECATED subGame' "subGame' is only kept for backwards compatibility, use subGame or the SubGamePA type instead" #-}
subGame' :: Foldable t => ParityGame a -> t Int -> (ParityGame a, Int -> Int, Int -> Int)
subGame' pa s = (newPA, fromJust . og, fromJust . toSubgraphNode)
    where
        (newPA, og, toSubgraphNode) = subGame pa s

newtype SubGamePA a = SubGamePA (ParityGame a,Set Int) deriving Show

instance ParityClass (SubGamePA a) where
    verticesPA (SubGamePA (pa,vs)) = filter (`Set.member` vs) (vertices (forgetPA pa))
    priorityPA (SubGamePA (pa,_)) = prioPA pa
    ownsPA (SubGamePA (pa,_)) = ownsPA pa
    successorsPA (SubGamePA (pa,vs)) n = Set.filter (`Set.member` vs) (successorsPA pa n)

instance Explorer (SubGamePA a) where
    type State _ = Int
    initStates (SubGamePA (_,vs)) = vs
    successors (SubGamePA (pa,vs)) n = Set.intersection vs (successors pa n)

subVertices :: SubGamePA a -> [Int]
subVertices (SubGamePA (pa,vs)) = filter (`Set.member` vs) (vertices (forgetPA pa))

attractorsStrat :: ParityClass a => a -> Set Int -> Player -> Set (Int, Int) -> (Set Int,Set (Int, Int))
attractorsStrat pa xs Even strat | xs == newXs = (xs, strat)
                                 | otherwise = attractorsStrat pa newXs Even (strat<>newStrat)
    where
        owns = ownsPA pa
        vs = verticesPA pa
        newXs = xs <> attracted
        attracted = foldl' (\play x -> 
            if not (Set.member x xs) 
                then (
                if owns x && any (`Set.member` xs) (successorsPA pa x) 
                    then Set.insert x play  
                    else  (
                if not (owns x) && all (`Set.member` xs) (successorsPA pa x) 
                    then Set.insert x play
                    else play
            )) else play) Set.empty vs
        newStrat = Set.map (\x -> (x,Set.findMax (Set.filter (\y -> Set.member y newXs && y /= x) $ successorsPA pa x))) 
                           (Set.filter owns attracted)
attractorsStrat pa xs Odd strat | xs == newXs = (xs, strat)
                                | otherwise = attractorsStrat pa newXs Odd (strat<>newStrat)
    where
        owns = ownsPA pa
        vs = verticesPA pa
        newXs = xs <> attracted
        attracted = foldl' (\play x -> 
            if not (Set.member x xs) 
                then (
                if not (owns x) && any (`Set.member` xs) (successorsPA pa x) 
                    then Set.insert x play  
                    else  (
                if owns x && all (`Set.member` xs) (successorsPA pa x) 
                    then Set.insert x play
                    else play
            )) else play) Set.empty vs
        newStrat = Set.map (\x -> (x,Set.findMax (Set.filter (\y -> Set.member y newXs && y /= x) $ successorsPA pa x))) 
                   (Set.filter (not . owns) attracted)


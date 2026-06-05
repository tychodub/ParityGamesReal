{-# LANGUAGE TypeFamilies #-}
module ParityGames.ParityArena where
import Data.Graph (Graph, vertices, edges, graphFromEdges)
import Explorer (Explorer(..), tarjanNontrivial)
import qualified Data.Set as Set
import Dot
import Data.Set (Set)
import qualified GHC.Arr as Array
import Data.Maybe (fromJust)
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Graph as Graph
import qualified Data.Foldable as Foldable
import Debug.Trace (traceShow, trace, traceShowId, traceShowWith)
import Data.Semigroup (Max(Max))

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
pruneLeafs :: ParityGame a -> ParityGame a 
pruneLeafs pa@(ArenaPA graph _ _ _) = newGraph
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
        (newGraph,_,_) = subGame pa leftoverVS

instance Show (ParityGame a) where
    show = show . forgetPA

instance Show a => Dot (ParityGame a) where
  dotNodes (ArenaPA graph pri evenOwn toNode) = Set.fromList $ map 
      (\x -> show (toNode x)++" [label = \""++show (toNode x)++", "++show (pri x)++"\"]"++ifEvenOwns x) (vertices graph)
      where
        ifEvenOwns x = if evenOwn x then "" else " [shape = pentagon]"
  dotArrows pa = Set.fromList (map (\(l,r) -> (show (indexToNode pa l),"\"\"",show (indexToNode pa r))) $ edges (forgetPA pa))
  dotName _ = "ParityArena"

instance Explorer (ParityGame a) where
    type State (ParityGame _) = Int
    initStates pa = Set.fromList $ vertices $ forgetPA pa
    successors pa n = if n `elem` (vertices (forgetPA pa)) then Set.fromList ((forgetPA pa) Array.! n)
                            else error ("subGame got incorrect index, n: "++show n++"\ngraph: "++show (forgetPA pa))

data Player = Even | Odd deriving (Show, Eq, Enum)

attractors :: ParityGame a -> Set Int -> Player -> Set Int
attractors pa@(ArenaPA graph _ owns _) xs Even | xs == newXs = xs
                                               | otherwise = attractors pa newXs Even
    where
        newXs = xs <> canPlay <> mustPlay
        canPlay = foldl' (\s (l,r) -> if r `Set.member` xs && owns l then Set.insert l s else s) Set.empty (edges graph)
        mustPlay = Set.fromList $ filter (\x -> all (`Set.member` xs) (successors pa x)) (vertices graph)
attractors pa@(ArenaPA graph _ owns _) xs Odd | xs == newXs = xs
                                              | otherwise = attractors pa newXs Odd
    where
        newXs = xs <> canPlay <> mustPlay
        canPlay = foldl' (\s (l,r) -> if r `Set.member` xs && not (owns l) then Set.insert l s else s) Set.empty (edges graph)
        mustPlay = Set.fromList $ filter (\x -> all (`Set.member` xs) (successors pa x)) (vertices graph)

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
        newStrat = Set.map (\x -> (x,head $ Set.toList (Set.filter (\y -> Set.member y xs) $ successorsPA pa x))) attracted
attractorsStrat pa xs Odd strat | xs == newXs = (xs, strat)
                                | otherwise = attractorsStrat pa newXs Odd newStrat
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
        newStrat = Set.map (\x -> (x,head $ Set.toList (Set.filter (\y -> Set.member y xs) $ successorsPA pa x))) attracted

-- | Tangle carries a set of nodes, a strategy and nodes it can escape to
newtype Tangle = Tangle (Set Int, Set (Int, Int), Set Int) deriving (Show, Eq, Ord)

tangleAttract :: ParityGame a -> Set Int -> Player -> Set (Int, Int) -> Set Tangle 
                 -> (Set Int, Set (Int, Int))
tangleAttract pa@(ArenaPA graph pri owns _) vs player oldStrat tangles = tangleAttractHelper vs (Set.toList vs) oldStrat
    where
        invertedGraph = Graph.transposeG graph
        predecessors n = invertedGraph Array.! n
        playerOwns Even = owns
        playerOwns Odd  = not . owns
        nodes = Set.fromList (vertices graph)
        tangleAttractHelper zSet []     strat = (zSet, strat)
        -- covered is to keep track of already processed vertices
        tangleAttractHelper zSet (x:xs) strat = tangleAttractHelper zSet'' xs'' strat''
            where
                xPredecessors = predecessors x
                (zSet',xs',strat') = foldl' (\(zSetIter,xsIter,stratIter) u -> let b1 = newPlay zSetIter u in
                    (insertIf b1 u zSetIter,insertIf2 b1 u xsIter, 
                    if playerOwns player u && u `Set.member` (insertIf b1 u zSetIter) 
                        && not (u `Set.member` (Set.map fst stratIter))
                        then Set.insert (u,x) stratIter 
                        else stratIter)) 
                        (zSet,xs,strat) xPredecessors 
                    where
                        newPlay zSetIter y = not (Set.member y zSetIter) && 
                                    (playerOwns player y ||
                                    ((successors pa y) `Set.isSubsetOf` zSetIter))
                        insertIf b y iterSet = if b then Set.insert y iterSet else iterSet
                        insertIf2 b y iterSet = if b then y:iterSet else iterSet
                (zSet'',xs'',strat'') = foldl' (\(zSetIter,xsIter,stratIter) (Tangle (tSet,tStrat,escape)) ->
                    if tSet `Set.isSubsetOf` (nodes <> zSetIter) && escape `Set.isSubsetOf` zSetIter 
                        then let u' = tSet Set.\\ zSetIter in (zSetIter<>tSet, 
                              Set.toList (Set.fromList xsIter<>u'),
                              stratIter<>(Set.filter (\(l,_) -> l `Set.member` u') tStrat)
                              )
                        else (zSetIter,xsIter,stratIter)) (zSet',xs',strat') 
                        (Set.filter (\(Tangle(tSet,_,_)) -> let (Max (_,v)) = (foldMap (\v' -> Max (pri v',v') ) tSet) 
                                                            in playerOwns player v) tangles) 

searchTangles :: ParityGame a -> Set Tangle -> Set Tangle 
searchTangles pa@(ArenaPA graph priority owns _) tangles | null (vertices graph) = Set.empty
                                                         | otherwise 
            = result
    where
        vsSet = Set.fromList (vertices graph)
        maxPriority = maximum (map priority (vertices graph))
        maxVertices = Set.filter (\v -> priority v == maxPriority) vsSet
        maxPlayer | even maxPriority = Even
                  | otherwise = Odd
        maxOwns | even maxPriority = owns
                | otherwise = not . owns
        (attractedZ, stratZ) = tangleAttract pa maxVertices maxPlayer Set.empty tangles
        (recursiveGraph, nodeFrom', vertexFrom') = subGame pa (vsSet Set.\\ attractedZ)
        vertexFrom = fromJust . vertexFrom'
        nodeFrom = fromJust . nodeFrom'
        recursiveTangles = Set.map (\(Tangle (l,r,escape)) -> 
            Tangle (Set.map vertexFrom l, Set.map (bimap vertexFrom vertexFrom) r, Set.map vertexFrom escape)) $
            Set.filter (\(Tangle (l,_,_)) -> not (any (`Set.member` attractedZ) l)) tangles -- check if filter really necessary
        result = if (Set.filter maxOwns attractedZ == Set.map fst stratZ) && 
                    (Set.unions $ Set.map (successors pa) (Set.filter (not . maxOwns) attractedZ)) `Set.isSubsetOf` attractedZ
                    then tangleUp (searchTangles recursiveGraph recursiveTangles) <> sccTangles
                    else tangleUp (searchTangles recursiveGraph recursiveTangles) 

        (zGraph,zNFrom,_) = Graph.graphFromEdges (map (\x -> (x,x,graph Array.! x)) (Set.toList attractedZ))
        bottomSCCs'' = Set.unions $ map (Set.fromList . Foldable.toList) $ map (tarjanNontrivial zGraph) (vertices zGraph)
        bottomSCCs' = Set.map (Set.map (\x -> let (a,_,_) = zNFrom x in a)) bottomSCCs''
        bottomSCCs = Set.filter (\tangle -> all (\x -> all (`Set.member` tangle) (successors pa x)) tangle) bottomSCCs'
        sccTangles = Set.map (\tangle -> Tangle (tangle, 
                                                 Set.filter (\(l,_) -> l `Set.member` tangle) stratZ,
                                                 Set.empty)) bottomSCCs -- no escape because bottom
        tangleUp = Set.map (\(Tangle (l,r,escape)) -> Tangle (Set.map nodeFrom l, 
                                                         Set.map (bimap nodeFrom nodeFrom) r, 
                                                         Set.map nodeFrom escape))

tangleLearning :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
tangleLearning pa = tangleLearning' pa Set.empty Set.empty Set.empty Set.empty Set.empty id

tangleLearning' :: ParityGame a -> Set Int -> Set Int -> Set (Int, Int) -> Set (Int, Int) -> Set Tangle -> (Int -> Int)
                                -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
tangleLearning' pa@(ArenaPA graph _ owns _) w0 w1 s0 s1 tangles og | null (vertices graph) = (w0,w1,s0,s1)
                                                                   | null noEscape = tangleLearning' pa w0 w1 s0 s1 tangles2 og
                                                                   | otherwise = 
        tangleLearning' newGraph (w0 <> nodesToOG evenRecurse) (w1 <> nodesToOG oddRecurse) 
                        (s0 <> stratToOG evenStratRecurse) (s1 <> stratToOG oddStratRecurse) newTangles newOG
    where
        setY = searchTangles pa tangles 
        (noEscape, withEscape) = Set.partition (\(Tangle (_,_,escape)) -> null escape) setY
        tangles2 = tangles <> withEscape -- this can stay empty forever in some graphs
        (evenNoEscape, oddNoEscape) = Set.partition (\(Tangle (_,tStrat,_))
                                          -> all owns (Set.map fst tStrat)) noEscape
        getNodes (Tangle (vs,_,_)) = vs
        getStrat (Tangle (_,strat,_)) = strat
        (evenRecurse, evenStratRecurse) = tangleAttract pa (Set.unions (Set.map getNodes evenNoEscape)) Even 
                                                           (Set.unions (Set.map getStrat evenNoEscape)) tangles2 
        (oddRecurse, oddStratRecurse) = tangleAttract pa (Set.unions (Set.map getNodes oddNoEscape)) Odd 
                                                         (Set.unions (Set.map getStrat oddNoEscape)) tangles2 
        (newGraph,nodeFrom,vertexFrom') = subGame pa (Set.fromList (vertices graph) Set.\\ (evenRecurse <> oddRecurse))
        vertexFrom = fromJust . vertexFrom'
        nodesToOG = Set.map og
        stratToOG = Set.map (bimap og og)
        --tangleToOg = Set.map (\(Tangle (a,b,c)) -> Tangle (nodesToOG a, stratToOG b, nodesToOG c)) -- for debugging
        nodesToNew = Set.map vertexFrom
        stratToNew = Set.map (bimap vertexFrom vertexFrom)
        newGraphVS = Set.fromList (vertices (forgetPA newGraph))
        newTangles = Set.filter (\(Tangle (nodes,_,_)) -> nodes `Set.isSubsetOf` newGraphVS) newTangles'
        newTangles' = Set.map (\(Tangle (a,b,c)) -> Tangle (nodesToNew a,stratToNew b,nodesToNew c)) tangles2
        newOG x = case nodeFrom x of Just y -> y; Nothing -> og x
        
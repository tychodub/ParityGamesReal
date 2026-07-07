module ParityGames.TangleLearning where
import ParityGames.ParityArena
import Data.Graph (vertices)
import Explorer (Explorer(..), tarjanNontrivial)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified GHC.Arr as Array
import Data.Maybe (fromJust)
import qualified Data.Graph as Graph
import qualified Data.Foldable as Foldable
import Data.Bifunctor (Bifunctor(..))
import Debug.Trace (traceShowId)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified GHC.Arr as Arr
import Utils.IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- | Tangle carries a set of nodes, a strategy and nodes it can escape to
newtype Tangle = Tangle (IntSet, IntMap Int, IntSet) deriving (Show, Eq, Ord)

tangleAttract :: ParityGame a -> IntSet -> Player -> IntMap Int -> Set Tangle 
                 -> (IntSet, IntMap Int)
tangleAttract (ArenaPA graph pri owns _) vs player oldStrat tangles = tangleAttractHelper vs (IntSet.toList vs) oldStrat
    where
        invertedGraph = Graph.transposeG graph
        predecessors n = invertedGraph Array.! n
        playerOwns Even = owns
        playerOwns Odd  = not . owns
        nodes = IntSet.fromList (vertices graph)
        tangleAttractHelper zSet []     strat = (zSet, strat)
        tangleAttractHelper zSet (x:xs) strat = tangleAttractHelper zSet'' xs'' strat''
            where
                xPredecessors = predecessors x
                (zSet',xs',strat') = foldl' (\(zSetIter,xsIter,stratIter) u -> let b1 = newPlay zSetIter u in
                    (insertIf b1 u zSetIter,insertIf2 b1 u xsIter, 
                    if playerOwns player u && u `IntSet.member` (insertIf b1 u zSetIter) 
                        && not (u `IntSet.member` (IntMap.keysSet stratIter))
                        then IntMap.insert u x stratIter 
                        else stratIter)) 
                        (zSet,xs,strat) xPredecessors 
                    where
                        newPlay zSetIter y = not (IntSet.member y zSetIter) && 
                                    (playerOwns player y ||
                                    ((IntSet.fromList $ graph Arr.! y) `IntSet.isSubsetOf` zSetIter))
                        insertIf b y iterSet = if b then IntSet.insert y iterSet else iterSet
                        insertIf2 b y iterSet = if b then y:iterSet else iterSet
                (zSet'',xs'',strat'') = foldl' (\(zSetIter,xsIter,stratIter) (Tangle (tSet,tStrat,escape)) ->
                    if tSet `IntSet.isSubsetOf` (nodes <> zSetIter) && escape `IntSet.isSubsetOf` zSetIter 
                        then let u' = tSet IntSet.\\ zSetIter in (zSetIter<>u', 
                              IntSet.toList (IntSet.fromList xsIter<>u'),
                              stratIter<>(IntMap.filterWithKey (\l _ -> l `IntSet.member` u') tStrat)
                              )
                        else (zSetIter,xsIter,stratIter)) (zSet',xs',strat') 
                        (Set.filter (\(Tangle(tSet,_,_)) -> playerOwns player $ IntSet.findMax (IntSet.map pri tSet)) tangles) 

searchTangles :: ParityGame a -> Set Tangle -> Set Tangle 
searchTangles pa@(ArenaPA graph priority owns _) tangles | null (vertices graph) = Set.empty
                                                         | otherwise 
            = result
    where
        intSuccs x = IntSet.fromList $ graph Arr.! x
        vsSet = IntSet.fromList (vertices graph)
        maxPriority = maximum (map priority (vertices graph))
        maxVertices = IntSet.filter (\v -> priority v == maxPriority) vsSet
        maxPlayer | even maxPriority = Even
                  | otherwise = Odd
        maxOwns | even maxPriority = owns
                | otherwise = not . owns
        (attractedZ, stratZ) = tangleAttract pa maxVertices maxPlayer IntMap.empty tangles
        (recursiveGraph, nodeFrom', vertexFrom') = subGame pa (vsSet IntSet.\\ attractedZ)
        vertexFrom = fromJust . vertexFrom'
        nodeFrom = fromJust . nodeFrom'
        recursiveTangles = Set.map (\(Tangle (l,r,escape)) -> 
            Tangle (IntSet.map vertexFrom l, IntMap.map vertexFrom $ IntMap.mapKeys vertexFrom r, IntSet.map vertexFrom escape)) tangles 
        result = if (IntSet.filter maxOwns attractedZ == IntMap.keysSet stratZ) && 
                    (flatMap intSuccs (IntSet.filter (not . maxOwns) attractedZ)) 
                                `IntSet.isSubsetOf` attractedZ
                    then tangleUp (searchTangles recursiveGraph recursiveTangles) <> sccTangles
                    else tangleUp (searchTangles recursiveGraph recursiveTangles) 

        (zGraph,zNFrom,_) = Graph.graphFromEdges (map (\x -> (x,x,graph Array.! x)) (IntSet.toList attractedZ))
        bottomSCCs'' = Set.unions $ map (Set.map toIntSet . Set.fromList . Foldable.toList) $ map (tarjanNontrivial zGraph) (vertices zGraph)
        bottomSCCs' = Set.map (IntSet.map (\x -> let (a,_,_) = zNFrom x in a)) bottomSCCs''
        bottomSCCs = Set.filter (\tangle -> all (\x -> maxOwns x || all (`IntSet.member` tangle) (successors pa x)) (IntSet.toList tangle)) bottomSCCs'
        sccTangles = Set.map (\tangle -> Tangle (tangle, 
                                                 IntMap.filterWithKey (\l _ -> l `IntSet.member` tangle) stratZ,
                                                 IntSet.empty -- INCORRECT PROBABLY
                                                 )) bottomSCCs 
        tangleUp = Set.map (\(Tangle (l,r,escape)) -> Tangle (IntSet.map nodeFrom l, 
                                                         IntMap.map nodeFrom $ IntMap.mapKeys nodeFrom r, 
                                                         IntSet.map nodeFrom escape))
        toIntSet = IntSet.fromDistinctAscList . Set.toAscList

tangleLearning :: ParityGame a -> (IntSet, IntSet, IntMap Int, IntMap Int)
tangleLearning pa = tangleLearning' pa IntSet.empty IntSet.empty IntMap.empty IntMap.empty Set.empty id

tangleLearning' :: ParityGame a -> IntSet -> IntSet -> IntMap Int -> IntMap Int -> Set Tangle -> (Int -> Int)
                                -> (IntSet, IntSet, IntMap Int, IntMap Int)
tangleLearning' pa@(ArenaPA graph priority _ _) w0 w1 s0 s1 tangles og | null (vertices graph) = (w0,w1,s0,s1)
                                                                       | null noEscape = tangleLearning' pa w0 w1 s0 s1 tangles2 og
                                                                       | otherwise = 
        tangleLearning' newGraph (traceShowId $ w0 <> nodesToOG evenRecurse) (traceShowId $ w1 <> nodesToOG oddRecurse) 
                        (s0 <> stratToOG evenStratRecurse) (s1 <> stratToOG oddStratRecurse) newTangles newOG
    where
        setY = searchTangles pa tangles 
        (noEscape, withEscape) = Set.partition (\(Tangle (_,_,escape)) -> IntSet.null escape) setY
        tangles2 = tangles <> withEscape 
        (evenNoEscape, oddNoEscape) = Set.partition (\(Tangle (tSet,_,_))
                                          -> even $ IntSet.findMax (IntSet.map priority tSet)) noEscape
        getNodes (Tangle (vs,_,_)) = vs
        getStrat (Tangle (_,strat,_)) = strat
        (evenRecurse, evenStratRecurse) = traceShowId $ tangleAttract pa (flatMapS getNodes evenNoEscape) Even 
                                                           (IntMap.unions (Set.map getStrat evenNoEscape)) tangles2 
        (oddRecurse, oddStratRecurse) = traceShowId $ tangleAttract pa (flatMapS getNodes oddNoEscape) Odd 
                                                         (IntMap.unions (Set.map getStrat oddNoEscape)) tangles2 
        (newGraph,nodeFrom,vertexFrom') = subGame pa (IntSet.fromList (vertices graph) IntSet.\\ (evenRecurse <> oddRecurse))
        vertexFrom = fromJust . vertexFrom'
        nodesToOG = IntSet.map og
        stratToOG = IntMap.map og . IntMap.mapKeys og
        --tangleToOg = Set.map (\(Tangle (a,b,c)) -> Tangle (nodesToOG a, stratToOG b, nodesToOG c)) -- for debugging
        nodesToNew = IntSet.map vertexFrom
        stratToNew = IntMap.map vertexFrom . IntMap.mapKeys vertexFrom
        newGraphVS = IntSet.fromList (vertices (forgetPA newGraph))
        newTangles = Set.filter (\(Tangle (nodes,_,_)) -> nodes `IntSet.isSubsetOf` newGraphVS) newTangles'
        newTangles' = Set.map (\(Tangle (a,b,c)) -> Tangle (nodesToNew a,stratToNew b,nodesToNew c)) tangles2
        newOG x = case nodeFrom x of Just y -> og y; Nothing -> og x 
        {-
        maxPri = maximum (vertices graph)
        maxPlayer | even maxPri = Even
                  | otherwise = Odd 
        (susAttract, susStrat) = tangleAttract pa (Set.filter (\x -> priority x == maxPri) $ Set.fromList (vertices graph)) maxPlayer Set.empty tangles
        (susGraph,nodeSus,vertexSus) = subGame pa (Set.fromList (vertices graph) Set.\\ susAttract)
        evenW | even maxPri = w0 <> susAttract
              | otherwise = w0
        oddW  | even maxPri = w1
              | otherwise = w1 <> susAttract
        evenS | even maxPri = s0 <> susStrat
              | otherwise = s0
        oddS  | even maxPri = s1
              | otherwise = s1 <> susStrat
        nodesToNew2 = Set.map (fromJust . vertexSus)
        stratToNew2 = Set.map (bimap vertexFrom vertexFrom)
        newGraphVS2 = Set.fromList (vertices (forgetPA susGraph))
        newTangles2 = Set.filter (\(Tangle (nodes,_,_)) -> nodes `Set.isSubsetOf` newGraphVS2) newTangles2'
        newTangles2' = Set.map (\(Tangle (a,b,c)) -> Tangle (nodesToNew2 a,stratToNew2 b,nodesToNew2 c)) tangles
        newOG2 x = case nodeSus x of Just y -> og y; Nothing -> og x 
-}

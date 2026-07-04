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
                        then let u' = tSet Set.\\ zSetIter in (zSetIter<>u', 
                              Set.toList (Set.fromList xsIter<>u'),
                              stratIter<>(Set.filter (\(l,_) -> l `Set.member` u') tStrat)
                              )
                        else (zSetIter,xsIter,stratIter)) (zSet',xs',strat') 
                        (Set.filter (\(Tangle(tSet,_,_)) -> playerOwns player $ maximum (Set.map pri tSet)) tangles) 

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
            Tangle (Set.map vertexFrom l, Set.map (bimap vertexFrom vertexFrom) r, Set.map vertexFrom escape)) tangles 
        result = if (Set.filter maxOwns attractedZ == Set.map fst stratZ) && 
                    (Set.unions $ Set.map (successors pa) (Set.filter (not . maxOwns) attractedZ)) `Set.isSubsetOf` attractedZ
                    then tangleUp (searchTangles recursiveGraph recursiveTangles) <> sccTangles
                    else tangleUp (searchTangles recursiveGraph recursiveTangles) 

        (zGraph,zNFrom,_) = Graph.graphFromEdges (map (\x -> (x,x,graph Array.! x)) (Set.toList attractedZ))
        bottomSCCs'' = Set.unions $ map (Set.fromList . Foldable.toList) $ map (tarjanNontrivial zGraph) (vertices zGraph)
        bottomSCCs' = Set.map (Set.map (\x -> let (a,_,_) = zNFrom x in a)) bottomSCCs''
        bottomSCCs = Set.filter (\tangle -> all (\x -> maxOwns x || all (`Set.member` tangle) (successors pa x)) tangle) bottomSCCs'
        sccTangles = Set.map (\tangle -> Tangle (tangle, 
                                                 Set.filter (\(l,_) -> l `Set.member` tangle) stratZ,
                                                 Set.empty -- INCORRECT PROBABLY
                                                 )) bottomSCCs 
        tangleUp = Set.map (\(Tangle (l,r,escape)) -> Tangle (Set.map nodeFrom l, 
                                                         Set.map (bimap nodeFrom nodeFrom) r, 
                                                         Set.map nodeFrom escape))

tangleLearning :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
tangleLearning pa = tangleLearning' pa Set.empty Set.empty Set.empty Set.empty Set.empty id

tangleLearning' :: ParityGame a -> Set Int -> Set Int -> Set (Int, Int) -> Set (Int, Int) -> Set Tangle -> (Int -> Int)
                                -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
tangleLearning' pa@(ArenaPA graph priority _ _) w0 w1 s0 s1 tangles og | null (vertices graph) = (w0,w1,s0,s1)
                                                                       | null noEscape = tangleLearning' pa w0 w1 s0 s1 tangles2 og
                                                                       | otherwise = 
        tangleLearning' newGraph (traceShowId $ w0 <> nodesToOG evenRecurse) (traceShowId $ w1 <> nodesToOG oddRecurse) 
                        (s0 <> stratToOG evenStratRecurse) (s1 <> stratToOG oddStratRecurse) newTangles newOG
    where
        setY = searchTangles pa tangles 
        (noEscape, withEscape) = Set.partition (\(Tangle (_,_,escape)) -> null escape) setY
        tangles2 = tangles <> withEscape 
        (evenNoEscape, oddNoEscape) = Set.partition (\(Tangle (tSet,_,_))
                                          -> even $ maximum (Set.map priority tSet)) noEscape
        getNodes (Tangle (vs,_,_)) = vs
        getStrat (Tangle (_,strat,_)) = strat
        (evenRecurse, evenStratRecurse) = traceShowId $ tangleAttract pa (Set.unions (Set.map getNodes evenNoEscape)) Even 
                                                           (Set.unions (Set.map getStrat evenNoEscape)) tangles2 
        (oddRecurse, oddStratRecurse) = traceShowId $ tangleAttract pa (Set.unions (Set.map getNodes oddNoEscape)) Odd 
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

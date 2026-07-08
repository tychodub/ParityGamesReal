module ParityGames.ForcedPath where
import ParityGames.ParityArena
import Data.Graph (transposeG, vertices)
import qualified GHC.Arr as Arr
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Explorer (Explorer(successors))
import qualified Data.IntMap.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

findForcedPaths :: ParityGame a -> Player -> IntSet -> Map.IntMap (Int, Maybe Int)
findForcedPaths pa pl target = forcedPathsHelper (Map.fromSet (const (0, Nothing)) target )
    where
        plOwns | pl == Even = ownsPA pa
               | otherwise  = not . ownsPA pa
        transposeGraph = transposeG (forgetPA pa)
        predecessors n = transposeGraph Arr.! n
        forcedPathsHelper foundPaths | null newPaths = foundPaths
                                     | otherwise = forcedPathsHelper resultPaths
            where
                newPathCandidates = IntSet.filter (\x -> (x `IntSet.member` target && isNothing (snd (foundPaths Map.! x))) 
                                  || not (IntSet.member x (Map.keysSet foundPaths))) 
                                  $ IntSet.unions (map (IntSet.fromList . predecessors) (Map.keys foundPaths))
                newPathsValid = IntSet.filter (\x -> if plOwns x 
                    then any (`IntSet.member` (Map.keysSet foundPaths)) (successors pa x)
                    else all (`IntSet.member` (Map.keysSet foundPaths)) (successors pa x)) newPathCandidates
                newPaths = Map.fromSet (\key -> 
                    minimum $ Set.map (\successor -> let (len,_) = foundPaths Map.! successor in (len+1,Just successor)) 
                    (Set.filter (`IntSet.member` (Map.keysSet foundPaths)) $ successors pa key)
                    ) newPathsValid
                resultPaths = newPaths <> foundPaths

-- | A Zielonka implementation that keeps track of the paths found by the attractor.
--   This information is used to avoid the initial recursive call in some cases where the \(B = W_{\overline{a}}\) branch
--   would be taken.
forcedPathZielonka :: ParityGame a -> (IntSet, IntSet,  IntMap Int, IntMap Int)
forcedPathZielonka pa = forcedPathZielonkaHelper pa id

findForcedPathsPartition :: ParityGame a -> Player -> IntSet -> (IntSet, IntMap Int, IntSet, IntMap Int)
findForcedPathsPartition pa pl uSet = (l,lStrat,r,rStrat)
    where
        -- l is known winner, r is undecided
        (l,lStrat,r,rStrat) = let found = findForcedPaths pa pl uSet in
            IntSet.foldl' (\(l',lStrat',r',rStrat') n ->
            if (case (found Map.!? n) of 
                Nothing -> error ("did not find "++show n++"in own attractor")
                Just (_,len) -> isJust len)
                then (l'<>IntSet.fromList (Map.keys found),
                lStrat'<>(IntMap.map (\(_,Just b) -> b) (IntMap.filter (isJust . snd) found)),
                r',rStrat') 
                else (l',lStrat',r'<>IntSet.fromList (Map.keys found),
                rStrat'<>IntMap.map (\(_,Just b) -> b) (IntMap.filter (isJust . snd) found)))
                (IntSet.empty, IntMap.empty, IntSet.empty, IntMap.empty) uSet
                 
                

forcedPathZielonkaHelper :: ParityGame a -> (Int -> Int) -> (IntSet, IntSet, IntMap Int, IntMap Int)
forcedPathZielonkaHelper pa@(ArenaPA graph pri _ _) og 
                    | null (vertices graph) = (IntSet.empty, IntSet.empty, IntMap.empty, IntMap.empty)
                    | IntSet.null uAttractUnd = if even maxPri 
                        then 
                        mapOG ((uAttractGood,IntSet.empty,sAGood,IntMap.empty)<>(forcedPathZielonkaHelper middleGraph og'))
                        else
                        mapOG ((IntSet.empty,uAttractGood,IntMap.empty,sAGood)<>(forcedPathZielonkaHelper middleGraph og'))
                    | complW == bAttract = if even maxPri 
                                                then (IntSet.map og (w0 <> uAttract), IntSet.map og w1,
                                                      IntMap.map og $ IntMap.mapKeys og (s0 <> sA <> picked0),
                                                      IntMap.map og $ IntMap.mapKeys og sB) 
                                                else (IntSet.map og w0,IntSet.map og (w1 <> uAttract),
                                                      IntMap.map og $ IntMap.mapKeys og sB, 
                                                      IntMap.map og $ IntMap.mapKeys og (s1 <> sA <> picked1))
                    | otherwise = if even maxPri 
                                                then (IntSet.map og w0',IntSet.map og (w1' <> bAttract), 
                                                      IntMap.map og $ IntMap.mapKeys og s0', 
                                                      IntMap.map og $ IntMap.mapKeys og (s1' <> sB)) 
                                                else (IntSet.map og (w0' <> bAttract),IntSet.map og w1', 
                                                      IntMap.map og $ IntMap.mapKeys og (s0' <> sB), 
                                                      IntMap.map og $ IntMap.mapKeys og s1')
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = IntSet.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        (uAttractGood,sAGood, uAttractUnd, sAUnd) = findForcedPathsPartition pa (playerFromInt maxPri) uSet
        sA = sAGood <> sAUnd
        uAttract = uAttractGood <> uAttractUnd
        (middleGraph,middleOG,_) = subGame pa (IntSet.fromList vs IntSet.\\ uAttractGood)
        og' x = case middleOG x of 
            Just y -> y
            Nothing -> error ("could not find og name of "++show x++", attractLoops: "++show uAttractGood)
        mapOG (res0,res1,resStrat0,resStrat1) = (IntSet.map og res0, IntSet.map og res1, 
                                                 IntMap.map og $ IntMap.mapKeys og resStrat0, 
                                                 IntMap.map og $ IntMap.mapKeys og resStrat1)
        vs' = IntSet.fromList vs IntSet.\\ (uAttractGood<>uAttractUnd)
        (newGraph,og3, _) = subGame' pa vs'
        (w0,w1,s0,s1) = forcedPathZielonkaHelper newGraph og3
        complW = if even maxPri then w1 else w0
        complS = if even maxPri then s1 else s0
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        (bAttract,sB) = attractorsStrat pa complW (playerFromIntFlipped maxPri) complS
        (ng2,og2,_) = subGame' pa (IntSet.fromList vs IntSet.\\ (bAttract))
        (w0',w1',s0',s1') = forcedPathZielonkaHelper ng2 og2
        (picked0,picked1) | even maxPri = IntMap.partitionWithKey (\l _ -> ownsPA pa l ) 
                                 (IntSet.foldl' (\m z -> IntMap.insert z 
                                    (Set.findMax (let tmp = successors pa z `Set.intersection` ((toSet w0) <> (toSet uAttract)) 
                                    in if null tmp then error ("could not find any "++show (successors pa z)++" in "++show ((toSet w0) <> (toSet uAttract))++" for "++show z) 
                                        else tmp)) m) IntMap.empty uAttractUnd)
                          | otherwise = IntMap.partitionWithKey (\l _ -> ownsPA pa l ) 
                                 (IntSet.foldl' (\m z -> IntMap.insert z
                                    (Set.findMax (let tmp = successors pa z `Set.intersection` ((toSet w1) <> (toSet uAttract)) 
                                    in if null tmp then error ("could not find any "++show (successors pa z)++" in "++show ((toSet w1) <> (toSet uAttract))++" for "++show z) 
                                        else tmp)) m) IntMap.empty uAttractUnd)
        toSet = Set.fromDistinctAscList . IntSet.toAscList
                                        

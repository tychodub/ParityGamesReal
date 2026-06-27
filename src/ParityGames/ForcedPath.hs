module ParityGames.ForcedPath where
import ParityGames.ParityArena
import Data.Set (Set)
import Data.Graph (transposeG, vertices)
import qualified GHC.Arr as Arr
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Explorer (Explorer(successors))
import Data.Bifunctor (bimap)
import ParityGames.Zielonka (zielonkaStrat)
import qualified Data.IntMap.Strict as Map
import Data.Maybe (isJust, isNothing)

findForcedPaths :: ParityGame a -> Player -> Int -> Map.IntMap (Int, Maybe Int)
findForcedPaths pa pl target = forcedPathsHelper (Map.singleton target (0, Nothing))
    where
        plOwns | pl == Even = ownsPA pa
               | otherwise  = not . ownsPA pa
        transposeGraph = transposeG (forgetPA pa)
        predecessors n = transposeGraph Arr.! n
        forcedPathsHelper foundPaths | null newPaths = foundPaths
                                     | otherwise = forcedPathsHelper resultPaths
            where
                newPathCandidates = IntSet.filter (\x -> (x == target && isNothing (snd (foundPaths Map.! target))) 
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
forcedPathZielonka :: ParityGame a -> (Set Int, Set Int,  Set (Int, Int), Set (Int, Int))
forcedPathZielonka pa = (\(a,b,c,d) -> 
                        (Set.fromAscList (IntSet.toAscList a),Set.fromAscList (IntSet.toAscList b),c,d)) 
                        $ forcedPathZielonkaHelper pa id

findForcedPathsPartition :: ParityGame a -> Player -> IntSet -> (IntSet, Set (Int,Int), IntSet, Set (Int, Int))
findForcedPathsPartition pa pl uSet = (l,lStrat,r,rStrat)
    where
        -- l is known winner, r is undecided
        (l,lStrat,r,rStrat) =  IntSet.foldr (\n (l',lStrat',r',rStrat') -> 
            let found = findForcedPaths pa pl n in
            if (case (found Map.!? n) of 
                Nothing -> error ("did not find "++show n++"in own attractor")
                Just (_,len) -> isJust len)
                then (l'<>IntSet.fromList (Map.keys found),
                lStrat'<>Set.fromList (map (\(a,(_,Just b)) -> (a,b)) (filter (isJust . snd . snd) (Map.toList found))),
                r',rStrat') 
                else (l',lStrat',r'<>IntSet.fromList (Map.keys found),
                rStrat'<>Set.fromList (map (\(a,(_,Just b)) -> (a,b)) (filter (isJust . snd . snd) (Map.toList found))))) 
                (IntSet.empty, Set.empty, IntSet.empty, Set.empty) uSet
                

forcedPathZielonkaHelper :: ParityGame a -> (Int -> Int) -> (IntSet, IntSet, Set (Int, Int), Set (Int, Int))
forcedPathZielonkaHelper pa@(ArenaPA graph pri _ _) og 
                    | null (vertices graph) = (IntSet.empty, IntSet.empty, Set.empty, Set.empty)
                    | IntSet.null uAttractUnd = if even maxPri 
                        then 
                        mapOG ((uAttractGood,IntSet.empty,sAGood,Set.empty)<>(forcedPathZielonkaHelper middleGraph og'))
                        else
                        mapOG ((IntSet.empty,uAttractGood,Set.empty,sAGood)<>(forcedPathZielonkaHelper middleGraph og'))
                    | otherwise = mapOG $ (\(a,b,c,d) -> 
                        (IntSet.fromAscList (Set.toAscList a),IntSet.fromAscList (Set.toAscList b),c,d)) $ zielonkaStrat pa
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = IntSet.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        (uAttractGood,sAGood, uAttractUnd, _) = findForcedPathsPartition pa (playerFromInt maxPri) uSet
        (middleGraph,middleOG,_) = subGame pa (IntSet.toList $ IntSet.fromList vs IntSet.\\ uAttractGood)
        og' x = case middleOG x of 
            Just y -> y
            Nothing -> error ("could not find og name of "++show x++", attractLoops: "++show uAttractGood)
        mapOG (res0,res1,resStrat0,resStrat1) = (IntSet.map og res0, IntSet.map og res1, 
                                                 Set.map (bimap og og) resStrat0, Set.map (bimap og og) resStrat1)
                                        

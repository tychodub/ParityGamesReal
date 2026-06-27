module ParityGames.ForcedPath where
import ParityGames.ParityArena
import Data.Set (Set)
import Data.Graph (transposeG, vertices)
import qualified GHC.Arr as Arr
import qualified Data.Set as Set
import Explorer (Explorer(successors))
import Data.Foldable (find)
import Data.Bifunctor (bimap)
import ParityGames.Zielonka (zielonkaVanDijkStrat)

-- currently does not optimize for shortest paths yet, but should be easy to implement if I have the time.
findForcedPaths :: ParityGame a -> Player -> Int -> Set (Int,[Int])
findForcedPaths pa pl target = forcedPathsHelper (Set.singleton (target,[]))
    where
        plOwns | pl == Even = ownsPA pa
               | otherwise  = not . ownsPA pa
        transposeGraph = transposeG (forgetPA pa)
        predecessors n = transposeGraph Arr.! n
        forcedPathsHelper foundPaths | null newPaths = foundPaths
                                     | otherwise = forcedPathsHelper resultPaths
            where
                newPathCandidates = Set.filter (\x -> not (Set.member x (Set.map fst foundPaths))) 
                                  $ Set.unions (Set.map (Set.fromList . predecessors . fst) foundPaths)
                newPathsValid = Set.filter (\x -> if plOwns x 
                    then any (`Set.member` (Set.map fst foundPaths)) (successors pa x)
                    else all (`Set.member` (Set.map fst foundPaths)) (successors pa x)) newPathCandidates
                newPaths = Set.map (\x -> let (Just (a,b)) = find (\(l,_) -> Set.member l (successors pa x)) foundPaths 
                                          in (x,a:b)) newPathsValid
                resultPaths = newPaths <> foundPaths

-- | A Zielonka implementation that keeps track of the paths found by the attractor.
--   This information is used to avoid the initial recursive call in some cases where the \(B = W_{\overline{a}}\) branch
--   would be taken.
--   Currently still slower in my benchmark, likely because of keeping track of the entire path and 
--   other inneficiencies introduced in this implemtation.
forcedPathZielonka :: ParityGame a -> (Set Int, Set Int,  Set (Int, Int), Set (Int, Int))
forcedPathZielonka pa = forcedPathZielonkaHelper pa id

findForcedPathsPartition :: ParityGame a -> Player -> Set Int -> (Set Int, Set (Int,Int), Set Int, Set (Int, Int))
findForcedPathsPartition pa pl uSet = (goodWin,goodStrat,otherWin,otherStrat)
    where
        (l,r) = bimap Set.unions Set.unions $ Set.foldr (\n (l',r') -> 
            let found = findForcedPaths pa pl n in
            if n `Set.member` Set.map fst (Set.filter (not . null . snd) found) 
                then (Set.insert found l',r') 
                else (l', Set.insert found r')) (Set.empty, Set.empty) uSet
        convertPathsToStrat pathSet = Set.foldr (\(x,xs) (l',r') -> 
            (Set.insert x l', if null xs then r' else Set.insert (x, head xs) r')) (Set.empty, Set.empty) pathSet
        (goodWin, goodStrat) = convertPathsToStrat l 
        (otherWin, otherStrat) = convertPathsToStrat r -- the undecided attractors

forcedPathZielonkaHelper :: ParityGame a -> (Int -> Int) -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
forcedPathZielonkaHelper pa@(ArenaPA graph pri _ _) og 
                    | null (vertices graph) = (Set.empty, Set.empty, Set.empty, Set.empty)
                    | null uAttractUnd = if even maxPri 
                        then 
                        mapOG ((uAttractGood,Set.empty,sAGood,Set.empty)<>(forcedPathZielonkaHelper middleGraph middleOG))
                        else
                        mapOG ((Set.empty,uAttractGood,Set.empty,sAGood)<>(forcedPathZielonkaHelper middleGraph middleOG))
                    | otherwise = mapOG $ zielonkaVanDijkStrat pa og
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = Set.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        (uAttractGood,sAGood, uAttractUnd, _) = findForcedPathsPartition pa (playerFromInt maxPri) uSet
        (middleGraph,middleOG,_) = subGame' pa (Set.fromList vs Set.\\ uAttractGood)
        mapOG (res0,res1,resStrat0,resStrat1) = (Set.map og res0, Set.map og res1, 
                                                 Set.map (bimap og og) resStrat0, Set.map (bimap og og) resStrat1)
                                        

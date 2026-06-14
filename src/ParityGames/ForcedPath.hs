module ParityGames.ForcedPath where
import ParityGames.ParityArena
import Data.Set (Set)
import Data.Graph (transposeG, vertices)
import qualified GHC.Arr as Arr
import qualified Data.Set as Set
import Explorer (Explorer(successors))
import Data.Foldable (find)

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

forcedPathZielonka :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
forcedPathZielonka paInitial = undefined
    where
        fpZielonkaHelper pa w0 w1 s0 s1 | null vs = (w0,w1,s0,s1)
                                        | even maxPri = fpZielonkaHelper undefined (w0<>Set.map fst forcedPaths) w1 nextSteps s1
            where
                vs = Set.fromList (vertices (forgetPA pa))
                priority = prioPA pa
                owns = ownsPA pa
                maxPri = maximum (Set.map priority vs)
                maxVS = Set.filter (\x -> priority x == maxPri) vs
                maxPlayer | even maxPri = Even
                          | otherwise   = Odd
                forcedPaths = Set.unions $ Set.map (findForcedPaths pa maxPlayer) maxVS
                nextSteps = foldl' (\strat (l,xs) -> undefined) s0 forcedPaths

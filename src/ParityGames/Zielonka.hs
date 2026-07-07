module ParityGames.Zielonka where

import ParityGames.ParityArena
import qualified Data.Set as Set
import Data.Graph
import Explorer (Explorer(..))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap (IntMap)

zielonka :: Ord a => ParityGame a -> (IntSet, IntSet)
zielonka pa = zielonkaVanDijk pa id id

zielonkaVanDijk :: ParityGame a -> (Int -> Int) -> (Int -> Int) -> (IntSet, IntSet)
zielonkaVanDijk pa@(ArenaPA graph pri _ _) og toCurrent | null (vertices graph) = (IntSet.empty, IntSet.empty)
                                                        | bAttract == complW = if even maxPri 
                                                                                then (w0<>IntSet.map og uAttract,w1) 
                                                                                else (w0,w1<>IntSet.map og uAttract)
                                                        | otherwise = if even maxPri 
                                                                       then (w0',w1'<>IntSet.map og bAttract) 
                                                                       else (w0'<>IntSet.map og bAttract,w1')
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = IntSet.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        uAttract = attractors pa uSet (playerFromInt maxPri) 
        vs' = IntSet.fromList vs IntSet.\\ uAttract
        (newGraph,og', toCurrent') = subGame' pa vs'
        (w0,w1) = zielonkaVanDijk newGraph (og . og') (toCurrent' . toCurrent)
        complW = if even maxPri then w1 else w0
        bAttract = attractors pa (IntSet.map toCurrent complW) (playerFromIntFlipped maxPri) -- we give total graph indices for non total graph
        (ng2,og2,tc2) = subGame' pa (IntSet.fromList vs IntSet.\\ bAttract)
        (w0',w1') = zielonkaVanDijk ng2 (og . og2) (tc2 . toCurrent)

zielonkaStrat :: ParityGame a -> (IntSet, IntSet,  IntMap Int, IntMap Int)
zielonkaStrat pa = zielonkaVanDijkStrat pa id

zielonkaVanDijkStrat :: ParityGame a -> (Int -> Int) -> (IntSet, IntSet, IntMap Int, IntMap Int)
zielonkaVanDijkStrat pa@(ArenaPA graph pri _ _) og 
                    | null (vertices graph) = (IntSet.empty, IntSet.empty, IntMap.empty, IntMap.empty)
                    | bAttract == complW = if even maxPri 
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
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        (uAttract,sA) = attractorsStrat pa uSet (playerFromInt maxPri) IntMap.empty
        vs' = IntSet.fromList vs IntSet.\\ uAttract
        (newGraph,og', _) = subGame' pa vs'
        (w0,w1,s0,s1) = zielonkaVanDijkStrat newGraph og'
        complW = if even maxPri then w1 else w0
        complS = if even maxPri then s1 else s0
        (bAttract,sB) = attractorsStrat pa complW (playerFromIntFlipped maxPri) complS
        (ng2,og2,_) = subGame' pa (IntSet.fromList vs IntSet.\\ (bAttract))
        (w0',w1',s0',s1') = zielonkaVanDijkStrat ng2 og2
        (picked0,picked1) | even maxPri = IntMap.partitionWithKey (\l _ -> ownsPA pa l ) 
                                 (IntSet.foldl' (\m z -> IntMap.insert z 
                                    (Set.findMax (successors pa z `Set.intersection` ((toSet w0) <> (toSet uAttract)))) m) IntMap.empty uSet)
                          | otherwise = IntMap.partitionWithKey (\l _ -> ownsPA pa l ) 
                                 (IntSet.foldl' (\m z -> IntMap.insert z
                                    (Set.findMax (successors pa z `Set.intersection` ((toSet w1) <> (toSet uAttract)))) m) IntMap.empty uSet)
        toSet = Set.fromDistinctAscList . IntSet.toAscList
      

module ParityGames.Zielonka where

import ParityGames.ParityArena
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (bimap)
import Data.Graph
import Explorer (Explorer(..))

zielonka :: Ord a => ParityGame a -> (Set a, Set a)
zielonka pa = bimap (Set.map (indexToNode pa)) (Set.map (indexToNode pa)) (zielonkaVanDijk pa id id)

zielonkaVanDijk :: ParityGame a -> (Int -> Int) -> (Int -> Int) -> (Set Int, Set Int)
zielonkaVanDijk pa@(ArenaPA graph pri _ _) og toCurrent | null (vertices graph) = (Set.empty, Set.empty)
                                                        | bAttract == complW = if even maxPri 
                                                                                then (w0<>Set.map og uAttract,w1) 
                                                                                else (w0,w1<>Set.map og uAttract)
                                                        | otherwise = if even maxPri 
                                                                       then (w0',w1'<>Set.map og bAttract) 
                                                                       else (w0'<>Set.map og bAttract,w1')
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = Set.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        uAttract = attractors pa uSet (playerFromInt maxPri) 
        vs' = Set.fromList vs Set.\\ uAttract
        (newGraph,og', toCurrent') = subGame' pa vs'
        (w0,w1) = zielonkaVanDijk newGraph (og . og') (toCurrent' . toCurrent)
        complW = if even maxPri then w1 else w0
        bAttract = attractors pa (Set.map toCurrent complW) (playerFromIntFlipped maxPri) -- we give total graph indices for non total graph
        (ng2,og2,tc2) = subGame' pa (Set.fromList vs Set.\\ bAttract)
        (w0',w1') = zielonkaVanDijk ng2 (og . og2) (tc2 . toCurrent)

zielonkaStrat :: ParityGame a -> (Set Int, Set Int,  Set (Int, Int), Set (Int, Int))
zielonkaStrat pa = zielonkaVanDijkStrat pa id

zielonkaVanDijkStrat :: ParityGame a -> (Int -> Int) -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
zielonkaVanDijkStrat pa@(ArenaPA graph pri _ _) og 
                    | null (vertices graph) = (Set.empty, Set.empty, Set.empty, Set.empty)
                    | bAttract == complW = if even maxPri 
                                                then (Set.map og (w0 <> uAttract), Set.map og w1,
                                                      Set.map (bimap og og) (s0 <> sA <> picked0),
                                                      Set.map (bimap og og) sB <> picked1) 
                                                else (Set.map og w0,Set.map og (w1 <> uAttract),
                                                      Set.map (bimap og og) sB <> picked0, 
                                                      Set.map (bimap og og) (s1 <> sA <> picked1))
                    | otherwise = if even maxPri 
                                                then (Set.map og w0',Set.map og (w1' <> bAttract), 
                                                      Set.map (bimap og og) s0', 
                                                      Set.map (bimap og og) (s1' <> sB)) 
                                                else (Set.map og (w0' <> bAttract),Set.map og w1', 
                                                      Set.map (bimap og og) (s0' <> sB), Set.map (bimap og og) s1')
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = Set.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        (uAttract,sA) = attractorsStrat pa uSet (playerFromInt maxPri) Set.empty
        vs' = Set.fromList vs Set.\\ uAttract
        (newGraph,og', _) = subGame' pa vs'
        (w0,w1,s0,s1) = zielonkaVanDijkStrat newGraph og'
        complW = if even maxPri then w1 else w0
        complS = if even maxPri then s1 else s0
        (bAttract,sB) = attractorsStrat pa complW (playerFromIntFlipped maxPri) complS
        (ng2,og2,_) = subGame' pa (Set.fromList vs Set.\\ bAttract)
        (w0',w1',s0',s1') = zielonkaVanDijkStrat ng2 og2
        (picked0,picked1) | even maxPri = Set.partition (\(l,_) -> ownsPA pa l ) 
                                 (Set.map (\z -> (z, Set.findMax (successors pa z `Set.intersection` (w0 <> uAttract)))) uSet)
                          | otherwise = Set.partition (\(l,_) -> ownsPA pa l ) 
                                 (Set.map (\z -> (z, Set.findMax (successors pa z `Set.intersection` (w1 <> uAttract)))) uSet)

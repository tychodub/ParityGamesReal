module ParityGames.FixedPointSolver where
import ParityGames.ParityArena
import qualified Data.Set as Set
import Data.Set (Set)
import Explorer (Explorer(..))
import Data.Graph (vertices)

winner :: ParityGame a -> Int -> Set Int -> Player
winner (ArenaPA _ priority _ _) v distractions | not (v `Set.member` distractions) = if even (priority v) then Even else Odd
                                               | otherwise = if even (priority v) then Odd else Even

oneStep :: ParityGame a -> Int -> Set Int -> Player
oneStep pa@(ArenaPA _ _ owns _) v distractions | owns v = if any (\u -> winner pa u distractions == Even) (successors pa v)
                                                             then Even
                                                             else Odd
                                               | otherwise = if any (\u -> winner pa u distractions == Odd) (successors pa v)
                                                                then Odd
                                                                else Even

oneStepDistraction :: ParityGame a -> Set Int -> Set Int
oneStepDistraction pa distractions = Set.filter (\v -> oneStep pa v distractions /= playerPri v) (Set.fromList (vertices (forgetPA pa)))
    where
        playerPri v | even (prioPA pa v) = Even
                    | otherwise = Odd

fpi :: ParityGame a -> (Set Int, Set Int)
fpi pa@(ArenaPA graph priority _ _) = fpiHelper Set.empty 0
    where
        vs = Set.fromList (vertices graph)
        vp p = Set.filter (\v -> priority v == p) vs
        maxPri = maximum (Set.map priority vs)
        fpiHelper distractions p | p > maxPri = (w0,vs Set.\\ w0)
                                 | null newDistract = fpiHelper distractions (p+1)
                                 | otherwise = fpiHelper (Set.filter (\v -> priority v >= p) (distractions <> newDistract)) 0
            where
                w0 = Set.filter (\v -> winner pa v distractions == Even) vs
                parity | even p = Even
                       | otherwise = Odd
                newDistract = Set.filter (\v -> (oneStep pa v distractions) /= parity) (vp p Set.\\ distractions)

fpiFreeze :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
fpiFreeze pa@(ArenaPA graph priority owns _) = fpiHelper Set.empty 0 Set.empty Set.empty Set.empty
    where
        vs = Set.fromList (vertices graph)
        vp p = Set.filter (\v -> priority v == p) vs
        maxPri = maximum (Set.map priority vs)
        fpiHelper distractions p frozen s0 s1 | p > maxPri = (w0,vs Set.\\ w0, s0, s1)
                                              | null newDistract = fpiHelper distractions (p+1) frozen s0' s1'
                                              | otherwise = fpiHelper newestDistract 0 newFrozen s0'' s1''
            where
                getEdgesPL distr v pl | winner pa v distr == pl = Set.singleton $ (v,Set.findMax $ Set.filter (\x -> winner pa x distr == pl) (successors pa v))
                                      | otherwise = Set.map (\x -> (v,x)) (successors pa v)
                nonfrozen = Set.filter (\v -> not (v `Set.member` (Set.map fst frozen))) (vp p)
                (s0',s1') = foldl' (\(s0Iter,s1Iter) v -> if owns v 
                    then (getEdgesPL distractions v Even <> s0Iter, s1Iter)
                    else (s0Iter, getEdgesPL distractions v Odd <> s1Iter)) 
                             (s0,s1) (nonfrozen Set.\\ distractions) 
                w0 = Set.filter (\v -> winner pa v distractions == Even) vs
                parity | even p = Even
                       | otherwise = Odd
                newDistract = Set.filter (\v -> (oneStep pa v distractions) /= parity) 
                                         (nonfrozen Set.\\ distractions) -- should it be nonfrozen or vp p? 
                combinedDistract = distractions <> newDistract 
                otherPlayerVS = Set.filter (\v -> priority v <= p && winner pa v combinedDistract /= parity) vs
                newFrozen' = 
                    (Set.map (\v -> (v,p)) $ Set.filter (\v -> all (\(l,_) -> v /= l) frozen) otherPlayerVS) 
                    <> 
                    (Set.map (\(l,r) -> if l `Set.member` otherPlayerVS && r <= p then (l,p) else (l,r)) frozen)
                (thawed,newFrozen) = Set.partition (\(l,r) -> winner pa l combinedDistract == parity && r < p) newFrozen'
                -- remove thawed from distractions and remove their strategies
                thawed' = (Set.map fst thawed)
                newestDistract = combinedDistract Set.\\ thawed'
                s0tmp = Set.filter (\(l,_) -> not (l `Set.member` thawed')) s0
                s1tmp = Set.filter (\(l,_) -> not (l `Set.member` thawed')) s1
                (newEvenDistract,newOddDistract) = Set.partition owns newDistract
                s0'' = s0tmp <> flatmapS (\x -> getEdgesPL newestDistract x Even) newEvenDistract
                s1'' = s1tmp <> flatmapS (\x -> getEdgesPL newestDistract x Odd) newOddDistract
                flatmapS f = Set.unions . Set.map f

-- | returns potentially non-deterministic strategies.
--   current implementation of justification graph works mostly fine usually, as long as the graph is locally small
--   (there are not many vertices originating from a single vertex). Could potentially be improved with an additional map
--   parameter to keep track of justified predecessors to turn O(n) into O(1) operation.
fpj :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
fpj pa@(ArenaPA graph priority owns _) = fpiHelper Set.empty 0 Set.empty
    where
        vs = Set.fromList (vertices graph)
        vp p = Set.filter (\v -> priority v == p) vs
        maxPri = maximum (Set.map priority vs)
        fpiHelper distractions p justified 
                                 | p > maxPri = (w0,vs Set.\\ w0, s0, s1)
                                 | null newDistract = fpiHelper distractions (p+1) (justified<>flatmapS (getEdges accumDistract) nonJustified)
                                 | otherwise = fpiHelper accumDistract 0 justified3
            where
                (s0,s1) = Set.partition (owns . fst) justified
                nonJustified = vp p Set.\\ Set.map fst justified
                getEdges distr v | owns v = getEdgesPL distr v Even
                                 | otherwise = getEdgesPL distr v Odd
                getEdgesPL distr v pl | winner pa v distr == pl = Set.singleton $ (v,Set.findMax $ Set.filter (\x -> winner pa x distr == pl) (successors pa v))
                                      | otherwise = Set.map (\x -> (v,x)) (successors pa v)
                w0 = Set.filter (\v -> winner pa v distractions == Even) vs
                parity | even p = Even
                       | otherwise = Odd
                newDistract = Set.filter (\v -> (oneStep pa v distractions) /= parity) (nonJustified Set.\\ distractions)
                justifiedPruned = pruneJustified justified newDistract
                pruneJustified accum flipped | null justFlipped = accum
                                             | otherwise = pruneJustified newAccum (Set.map fst justFlipped)
                                       where
                                        (justFlipped,midAccum) = Set.partition (\(_,r) -> r `Set.member` flipped) accum
                                        flippedVS = Set.map fst justFlipped
                                        newAccum = Set.filter (\(l,_) -> not (l `Set.member` flippedVS)) midAccum
                noMoreJustified = justified Set.\\ justifiedPruned
                accumDistract = (distractions Set.\\ (Set.map fst noMoreJustified)) <> newDistract
                justified3 = justifiedPruned <> flatmapS (getEdges accumDistract) newDistract
                flatmapS f = Set.unions . Set.map f
                


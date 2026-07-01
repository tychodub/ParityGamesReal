module ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj) where
import ParityGames.ParityArena
import qualified Data.Set as Set
import Data.Set (Set)
import Explorer (Explorer(..))
import Data.Graph (vertices)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Bifunctor (Bifunctor(bimap))

toSet :: IntSet -> Set Int
toSet = Set.fromAscList . IntSet.toAscList

winner :: ParityGame a -> Int -> IntSet -> Player
winner (ArenaPA _ priority _ _) v distractions | not (v `IntSet.member` distractions) = if even (priority v) then Even else Odd
                                               | otherwise = if even (priority v) then Odd else Even

oneStep :: ParityGame a -> Int -> IntSet -> Player
oneStep pa@(ArenaPA _ _ owns _) v distractions | owns v = if any (\u -> winner pa u distractions == Even) (successors pa v)
                                                             then Even
                                                             else Odd
                                               | otherwise = if any (\u -> winner pa u distractions == Odd) (successors pa v)
                                                                then Odd
                                                                else Even

oneStepDistraction :: ParityGame a -> IntSet -> IntSet
oneStepDistraction pa distractions = IntSet.filter (\v -> oneStep pa v distractions /= playerPri v) 
                                                   (IntSet.fromList (vertices (forgetPA pa)))
    where
        playerPri v | even (prioPA pa v) = Even
                    | otherwise = Odd

fpi :: ParityGame a -> (Set Int, Set Int)
fpi pa@(ArenaPA graph priority _ _) = bimap toSet toSet $ fpiHelper IntSet.empty 0
    where
        vs = IntSet.fromList (vertices graph)
        vp p = IntSet.filter (\v -> priority v == p) vs
        maxPri = IntSet.findMax (IntSet.map priority vs)
        fpiHelper distractions p | p > maxPri = (w0,vs IntSet.\\ w0)
                                 | IntSet.null newDistract = fpiHelper distractions (p+1)
                                 | otherwise = fpiHelper (IntSet.filter (\v -> priority v >= p) (distractions <> newDistract)) 0
            where
                w0 = IntSet.filter (\v -> winner pa v distractions == Even) vs
                parity | even p = Even
                       | otherwise = Odd
                newDistract = IntSet.filter (\v -> (oneStep pa v distractions) /= parity) (vp p IntSet.\\ distractions)

fpiFreeze :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
fpiFreeze pa@(ArenaPA graph priority owns _) = (toSet res0, toSet res1, resStrat0, resStrat1)
    where
        (res0,res1,resStrat0,resStrat1) = fpiHelper IntSet.empty 0 Set.empty Set.empty Set.empty
        vs = IntSet.fromList (vertices graph)
        vp p = IntSet.filter (\v -> priority v == p) vs
        maxPri = IntSet.findMax (IntSet.map priority vs)
        fpiHelper distractions p frozen s0 s1 | p > maxPri = (w0,vs IntSet.\\ w0, s0, s1)
                                              | IntSet.null newDistract = fpiHelper distractions (p+1) frozen s0' s1'
                                              | otherwise = fpiHelper newestDistract 0 newFrozen s0'' s1''
            where
                getEdgesPL distr oldDistr v pl | winner pa v distr == pl = Set.singleton $ (v,
                                            Set.findMax $ Set.filter (\x -> winner pa x oldDistr == pl) (successors pa v))
                                               | otherwise = Set.empty
                nonfrozen = IntSet.filter (\v -> not (v `Set.member` (Set.map fst frozen))) (vp p)
                (s0',s1') = IntSet.foldl' (\(s0Iter,s1Iter) v -> if owns v 
                    then (getEdgesPL distractions distractions v Even <> s0Iter, s1Iter)
                    else (s0Iter, getEdgesPL distractions distractions v Odd <> s1Iter)) 
                             (s0,s1) (nonfrozen IntSet.\\ distractions) 
                w0 = IntSet.filter (\v -> winner pa v distractions == Even) vs
                parity | even p = Even
                       | otherwise = Odd
                newDistract = IntSet.filter (\v -> (oneStep pa v distractions) /= parity) 
                                         (nonfrozen IntSet.\\ distractions) 
                combinedDistract = distractions <> newDistract 
                otherPlayerVS = IntSet.filter (\v -> priority v <= p && winner pa v combinedDistract /= parity) vs
                newFrozen' = 
                    (Set.map (\v -> (v,p)) $ toSet $ IntSet.filter (\v -> all (\(l,_) -> v /= l) frozen) otherPlayerVS) 
                    <> 
                    (Set.map (\(l,r) -> if l `IntSet.member` otherPlayerVS && r <= p then (l,p) else (l,r)) frozen)
                (thawed,newFrozen) = Set.partition (\(l,r) -> winner pa l combinedDistract == parity && r < p) newFrozen'
                -- remove thawed from distractions and remove their strategies
                thawed' = IntSet.fromDistinctAscList $ Set.toAscList (Set.map fst thawed)
                newestDistract = combinedDistract IntSet.\\ thawed'
                s0tmp = Set.filter (\(l,_) -> not (l `IntSet.member` thawed')) s0
                s1tmp = Set.filter (\(l,_) -> not (l `IntSet.member` thawed')) s1
                (newEvenDistract,newOddDistract) = Set.partition owns (toSet newDistract)
                s0'' = s0tmp <> flatmapS (\x -> getEdgesPL newestDistract distractions x Even) newEvenDistract
                s1'' = s1tmp <> flatmapS (\x -> getEdgesPL newestDistract distractions x Odd) newOddDistract
                flatmapS f = Set.unions . Set.map f

-- | current implementation of justification graph works mostly fine usually, as long as the graph is locally small
--   (there are not many vertices originating from a single vertex). Could potentially be improved with an additional map
--   parameter to keep track of justified predecessors to turn O(n) into O(1) operation.
fpj :: ParityGame a -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
fpj pa@(ArenaPA graph priority owns _) = (toSet res0, toSet res1, resStrat0, resStrat1)
    where
        (res0,res1,resStrat0,resStrat1) = fpiHelper IntSet.empty 0 Set.empty
        vs = IntSet.fromList (vertices graph)
        vp p = IntSet.filter (\v -> priority v == p) vs
        maxPri = IntSet.findMax (IntSet.map priority vs)
        fpiHelper distractions p justified 
                                 | p > maxPri = (w0,vs IntSet.\\ w0, s0, s1)
                                 | IntSet.null newDistract = fpiHelper distractions (p+1) (justified<>flatmapS (getEdges accumDistract accumDistract) (toSet nonJustified))
                                 | otherwise = fpiHelper accumDistract 0 justified3
            where
                (s0,s1) = Set.partition (owns . fst) justified
                nonJustified = vp p IntSet.\\ (IntSet.fromDistinctAscList $ Set.toAscList $ Set.map fst justified)
                getEdges distr oldDistr v | owns v = getEdgesPL distr oldDistr v Even
                                          | otherwise = getEdgesPL distr oldDistr v Odd
                -- Set.findMax is not right because what if it goes to itself now that it wins, but going to itself is bad
                getEdgesPL distr oldDistr v pl | winner pa v distr == pl = Set.singleton $ (v,
                                         Set.findMax $ Set.filter (\x -> winner pa x oldDistr == pl) (successors pa v))
                                               | otherwise = Set.map (\x -> (v,x)) (successors pa v)
                w0 = IntSet.filter (\v -> winner pa v distractions == Even) vs
                parity | even p = Even
                       | otherwise = Odd
                newDistract = IntSet.filter (\v -> (oneStep pa v distractions) /= parity) (nonJustified IntSet.\\ distractions)
                justifiedPruned = pruneJustified justified newDistract
                pruneJustified accum flipped | null justFlipped = accum
                                             | otherwise = pruneJustified newAccum (IntSet.fromDistinctAscList 
                                                                   $ Set.toAscList $ Set.map fst justFlipped)
                                       where
                                        (justFlipped,midAccum) = Set.partition (\(_,r) -> r `IntSet.member` flipped) accum
                                        flippedVS = Set.map fst justFlipped
                                        newAccum = Set.filter (\(l,_) -> not (l `Set.member` flippedVS)) midAccum
                noMoreJustified = justified Set.\\ justifiedPruned
                distractWithoutNew = (distractions IntSet.\\ (IntSet.fromDistinctAscList $ Set.toAscList $ 
                                                              Set.map fst noMoreJustified))
                accumDistract = distractWithoutNew <> newDistract
                justified3 = justifiedPruned <> flatmapS (getEdges accumDistract distractions) (toSet newDistract)
                flatmapS f = Set.unions . Set.map f
                


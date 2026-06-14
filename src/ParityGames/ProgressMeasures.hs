module ParityGames.ProgressMeasures where
import ParityGames.ParityArena
import Data.Graph (vertices, transposeG)
import Data.Set (Set, partition)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Foldable (find)
import Explorer (Explorer(successors))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.IntMap.Strict as Map
import GHC.Arr ((!))
import Debug.Trace (traceShowId, traceShow, trace)

newtype Progress = Pr (Maybe (Seq Int)) deriving (Show, Eq)
type ProgressMeasure = Int -> Progress -- might be worth turning into IntMap, but benchmark!

prApp :: (Seq Int -> Seq Int) -> Progress -> Progress
prApp f (Pr m) = Pr (fmap f m)

prWrap :: Seq Int -> Progress
prWrap = Pr . Just

zeroMeasure :: ParityGame a -> Player -> ProgressMeasure
zeroMeasure pa pl _ = Pr $ Just $ Seq.replicate (length $ (playerRange pa pl)) 0

instance Ord Progress where
    _ <= (Pr Nothing) = True
    (Pr Nothing) <= _ = False
    (Pr (Just xs)) <= (Pr (Just ys)) = xs <= ys

instance Semigroup Progress where
    (Pr Nothing) <> _ = Pr Nothing
    _ <> (Pr Nothing) = Pr Nothing
    (Pr (Just xs)) <> (Pr (Just ys)) = Pr (Just (xs<>ys))

instance Monoid Progress where
    mempty = prWrap Seq.empty

playerRange :: ParityGame a -> Player -> Seq (Int, Int)
playerRange (ArenaPA graph priority _ _) pl = Seq.fromList $ reverse (Map.toList accumSet)
    where
        notPri | pl == Even = odd . priority
               | otherwise  = even . priority
        accumSet = foldl' (\state v -> if notPri v 
            then Map.insertWith (+) (priority v) 1 state 
            else state) Map.empty (vertices graph)

overflowIncr :: Seq (Int, Int) -> Progress -> Progress
overflowIncr _ (Pr Nothing) = Pr Nothing
overflowIncr range (Pr (Just xs)) | fst result = Pr Nothing
                                  | otherwise  = prWrap $ snd result
    where
        incrVal i n = (n+1) `rem` (1+case fmap snd $ Seq.lookup i range of 
            Nothing -> error ("overflowIncr index error, xs: "++show xs++", i: "++show i++", range: "++show range)
            Just x -> x)
        indexCalc i n (overflowed,xs') = if overflowed 
            then (if (incrVal i n) == 0 then True else False,Seq.update i (incrVal i n) xs') 
            else (False,xs')
        result = Seq.foldrWithIndex indexCalc (True,xs) xs

prog :: Seq (Int,Int) -> Progress -> Int -> Player -> Progress
prog _ (Pr Nothing) _ _ = Pr Nothing
prog range (Pr (Just m)) p player | pPlayer == player = Pr (Just case1Result)
                                  | otherwise = case2Result
    where
        pPlayer = toEnum (p `rem` 2)
        (prefix,toReset) = Seq.splitAt splitIndex m
        case1Result = prefix <> Seq.replicate (length toReset) 0
        case2Result = (overflowIncr range (prWrap prefix))<>prWrap (Seq.replicate (length toReset) 0)
        splitIndex = case Seq.findIndexL (\(l,_) -> l < p) range of
                          Just x  -> x 
                          Nothing -> length m

lift :: ParityGame a -> Seq (Int, Int) -> ProgressMeasure -> Int -> Player -> Progress
lift pa range f v pl | plOwns v = minimum candidates
                     | otherwise = maximum candidates
    where
        vSuccs = successors pa v
        plOwns | pl == Even = ownsPA pa
               | otherwise = not . ownsPA pa
        priority = priorityPA pa
        candidates = Set.map (\w -> prog range (f w) (priority v) pl) vSuccs

class LiftStrat a where
    lifted :: ParityGame b -> a -> Int -> a
    nextV :: ParityGame b -> a -> (Maybe Int,a)

spmSlides ::  ParityGame a -> Player -> (Set Int, Set Int, Set (Int, Int))
spmSlides pa pl = (w0,w1,strat)
    where
        vertexSet = Set.fromList (verticesPA pa)
        range = playerRange pa pl
        invertedGraph = Data.Graph.transposeG (forgetPA pa)
        predecessors n = invertedGraph ! n
        initMeasure = zeroMeasure pa pl
        initQueue = filter (\v -> toEnum (priorityPA pa v `rem` 2) /= pl) (verticesPA pa)
        loopHelper [] spm = spm
        loopHelper (x:xs) spm = if (spm x) < (lift pa range spm x pl)
            -- predecessors may add something that's already in the queue, which is technically not optimal
            then loopHelper (predecessors x++xs) newSPM 
            else loopHelper xs spm
            where
                newSPM v | v == x = lift pa range spm x pl
                         | otherwise = spm v
        resultSPM = loopHelper initQueue initMeasure
        (w0,w1) | pl == Even = Set.partition (\v -> resultSPM v /= Pr Nothing) vertexSet
                | otherwise  = Set.partition (\v -> resultSPM v == Pr Nothing) vertexSet
        plOwns | pl == Even = ownsPA pa
               | otherwise = not . ownsPA pa
        pickSucc v = Set.findMax (Set.filter (\u -> resultSPM v == prog range (resultSPM u) (prioPA pa v) pl) (successors pa v))
        strat | pl == Even = Set.map (\v -> (v,pickSucc v)) (Set.intersection w0 (Set.filter plOwns vertexSet))
              | otherwise  = Set.map (\v -> (v,pickSucc v)) (Set.intersection w1 (Set.filter plOwns vertexSet))

newtype LinearLiftStrat = LLS (Int, Int) deriving (Show, Eq)

llsFromPA :: ParityGame a -> LinearLiftStrat
llsFromPA (ArenaPA graph _ _ _) = LLS (0,length (vertices graph))

instance LiftStrat LinearLiftStrat where
    lifted _ (LLS (_,r)) _ = LLS (0,r)
    nextV pa strat@(LLS (failed,r)) | failed >= lenV = (Nothing,strat)
                                    | otherwise = (Just (vertices (forgetPA pa)!!nextVertex), LLS (failed+1,nextVertex))
        where
            lenV = (length . vertices . forgetPA) pa
            nextVertex = (r + 1) `rem` lenV

newtype SubStrat a = SubS (Set Int, a) deriving (Show, Eq)

instance (LiftStrat a) => LiftStrat (SubStrat a) where
    lifted pa (SubS (w,strat)) v = SubS (w,lifted pa strat v)
    nextV pa (SubS (w,strat)) | focussedV == Nothing = (Nothing, SubS (w,newStrat))
                              | fromJust focussedV `Set.member` w = (focussedV, SubS (w,newStrat))
                              | otherwise = nextV pa (SubS (w,newStrat))
        where
            (focussedV, newStrat) = nextV pa strat

-- | assumes u is a subset of `Set.filter (\x -> x >= k) w`.
--   the k parameter from the paper is not in the function since it is really an assumption on u.
guardedAttractors :: ParityArena -> Player -> Set Int -> Set Int -> Set Int
guardedAttractors pa@(ArenaPA graph pri _ _) pl w u = plAdmitted u
    where
        plAdmitted u' | u' == newU = u'
                      | otherwise = plAdmitted u'
            where
                newU = u' <> (Set.filter (\x -> if playerSwitch pl (pri x) 
                                          then (successors pa x `Set.intersection` u') /= Set.empty
                                          else (successors pa x `Set.intersection` w) `Set.isSubsetOf` u') 
                                          (Set.fromList $ vertices graph))
        playerSwitch Even = even -- check if still correct
        playerSwitch Odd  = odd -- ditto


-- | based on the modified algorithm in https://arxiv.org/pdf/1509.07207
gazdaWillemseSPMPartition :: LiftStrat a => ParityArena -> a -> (Set Int, Set Int)
gazdaWillemseSPMPartition pa@(ArenaPA graph _ _ _) strat = partition (\v -> spm v /= Pr Nothing) (Set.fromList (vertices graph))
    where
        spm = gazdaWillemseSPM pa strat

gazdaWillemseSPM :: LiftStrat a => ParityArena -> a -> ProgressMeasure
gazdaWillemseSPM pa@(ArenaPA graph _ _ _) strat = spmWithin pa mt (Set.fromList $ vertices graph) (zeroMeasure pa Even) strat
    where
        mt = playerRange pa Even

spmWithin :: LiftStrat a => ParityArena -> Seq (Int, Int) -> Set Int -> ProgressMeasure -> a -> ProgressMeasure
spmWithin pa mt w spm strat | null w = spm
                         | otherwise = let ((spm',strat',a),b) = (innerLoop spm initStrat initV) 
                         in if b then spm' else spmWithin pa mt (w Set.\\ a) spm' strat'
    where
        (initV, initStrat) = nextV pa strat
        innerLoop spm' strat' Nothing = breakCond spm' strat'
        innerLoop spm' strat' (Just _) | all (\x -> spm' x /= Pr Nothing) w = innerLoop newSPM newStrat' newV
                                       | otherwise = breakCond spm' strat'
            where
                newSPM = (\x -> if spm' x < lift pa mt spm' x Even then lift pa mt spm' x Even else spm' x)
                newStrat = Set.foldl' (\s x -> lifted pa s x) strat' w
                (newV,newStrat') = nextV pa newStrat
        breakCond spm' strat' = case find (\x -> spm' x == Pr Nothing) w of 
                                      Nothing -> ((spm',strat', Set.empty), True) -- the returned set is a dummy
                                      Just v  -> (beforeForAll1 pa mt w spm' strat' v, False)

beforeForAll1 :: LiftStrat a => ParityArena -> Seq (Int, Int) -> Set Int -> ProgressMeasure -> a -> Int 
                 -> (ProgressMeasure, a, Set Int)
beforeForAll1 pa@(ArenaPA _ pri _ _) precomputedRange w spm strat v = (spmNew3, strat, a)
    where
        k = pri v
        -- sigma is for computing winning strategies, not of interest for now
        --sigma = foldl' (\s x -> if pri x > pri s then x else s) minBound (Set.intersection (successors pa v) (Set.fromList $ toList w'))
        res1 = guardedAttractors pa Even w (Set.singleton v)
        spmNew1 = \x -> if x `Set.member` (Set.delete v res1) then Pr Nothing else spm x
        irr1 = guardedAttractors pa Odd w (Set.filter (\x -> pri x < k) w)
        rem1 = w Set.\\ (res1 <> irr1)
        spmNew2 = spmWithin pa precomputedRange rem1 spmNew1 (SubS (rem1,strat)) 
        dom = res1 <> (Set.filter (\x -> spmNew2 x == Pr Nothing) res1)
        a = guardedAttractors pa Even w dom
        outerA = a Set.\\ dom 
        spmNew3 = \x -> if x `Set.member` outerA then Pr Nothing else spmNew2 x
        
-- End section gazdaWillemse

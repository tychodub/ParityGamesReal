module ProgressMeasures where
import ParityArena
import qualified Data.Graph as Graph
import Data.Graph (vertices, edges)
import Data.Set (Set, partition)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Foldable (find, Foldable (toList))
import Explorer (Explorer(successors))
import Debug.Trace (trace)
import Control.Applicative ((<**>))

newtype Progress = Pr (Maybe [Int]) deriving (Show, Eq)
type ProgressMeasure = Int -> Progress

prApp :: ([Int] -> [Int]) -> Progress -> Progress
prApp f (Pr m) = Pr (fmap f m)

prWrap :: [Int] -> Progress
prWrap = Pr . Just

zeroMeasure :: ParityArena -> ProgressMeasure
zeroMeasure (ArenaPA graph _ _) _ = Pr $ Just $ replicate (length $ vertices graph) 0

instance Ord Progress where
    _ <= (Pr Nothing) = True
    (Pr Nothing) <= _ = False
    (Pr (Just xs)) <= (Pr (Just ys)) = xs <= ys

smallRange :: ParityArena -> Set Progress
smallRange (ArenaPA graph pri _) = Set.insert (Pr Nothing) (Set.fromList $ map prWrap (xs k))
    where
        k = length (Set.fromList (fmap pri (vertices graph)))
        xs 0 = [[0]]
        xs i | even i =  xs (i-1)<**>[\ys -> ys++[0]]
             | otherwise = (++) <$> xs (i-1)<*>(fmap pure [0..n])
             where
                n = length (filter (\x -> pri x == i) (vertices graph))



-- | assumes that the progress measure is small.
--   This is a horrible algorithm that recomputes the range of the progress measure each time.
prog :: ParityArena -> Set Progress -> ProgressMeasure -> Graph.Edge -> Player -> Progress
prog (ArenaPA _ pri _) sR f (l',r) pl = case f r of 
                      (Pr Nothing) -> Pr Nothing
                      (Pr (Just r')) -> case2 r'
    where
        l = pri l'
        case2 r' | playerSwitch pl l = minimum (Set.filter (\m -> prApp (take l) m >= prWrap (take l r')) sR)
                 | otherwise = minimum (Set.filter (\m -> prApp (take l) m > prWrap (take l r')) sR)
        playerSwitch Even = even -- check if still correct
        playerSwitch Odd  = odd  -- ditto

lift :: ParityArena -> Set Progress -> ProgressMeasure -> Int -> Player -> ProgressMeasure
lift pa mt f v pl u | u /= v = f u
                    | flippedPLSwitch pl (prioPA pa v) = max (f v) (minimum [prog pa mt f e pl | e <- edges (forgetPA pa)])
                    | otherwise = max (f v) (maximum [prog pa mt f e pl | e <- edges (forgetPA pa)])
    where
        flippedPLSwitch Even = odd -- check if still correct
        flippedPLSwitch Odd  = even -- ditto

class LiftStrat a where
    lifted :: ParityArena -> a -> Int -> a
    nextV :: ParityArena -> a -> (Maybe Int,a)

spmFixedPoint :: LiftStrat a => ParityArena -> ProgressMeasure -> a -> ProgressMeasure
spmFixedPoint pa f strat = spmFPHelper f v strat'
    where
        mt = smallRange pa
        (v, strat') = nextV pa strat 
        spmFPHelper spm Nothing _ = spm 
        spmFPHelper spm (Just v') strat'' = spmFPHelper spm' v'' newStrat''
            where
                newSPM = lift pa mt spm v' Odd
                (spm',newStrat) = if spm v' > newSPM v'
                                     then (newSPM,lifted pa strat'' v')
                                     else (spm,strat'')
                (v'', newStrat'') = (nextV pa newStrat)

newtype LinearLiftStrat = LLS (Int, Int) deriving (Show, Eq)

llsFromPA :: ParityArena -> LinearLiftStrat
llsFromPA (ArenaPA graph _ _) = LLS (0,length (vertices graph))

instance LiftStrat LinearLiftStrat where
    lifted _ (LLS (_,r)) _ = LLS (0,r)
    nextV pa strat@(LLS (failed,r)) | failed >= lenV = (Nothing,strat)
                                    | otherwise = (Just (vertices (forgetPA pa)!!nextVertex), LLS (failed+1,nextVertex))
        where
            lenV = (length . vertices . forgetPA) pa
            nextVertex = (r + 1) `rem` lenV

spmBasic :: LiftStrat a => ParityArena -> a -> (Set Int, Set Int)
spmBasic pa@(ArenaPA graph _ _) strat = partition (\v -> newMeasure v /= Pr Nothing) (Set.fromList (vertices graph))
    where
        newMeasure = spmFixedPoint pa (zeroMeasure pa) strat

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
guardedAttractors pa@(ArenaPA graph pri _) pl w u = plAdmitted u
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
gazdaWillemseSPMPartition pa@(ArenaPA graph _ _) strat = partition (\v -> spm v /= Pr Nothing) (Set.fromList (vertices graph))
    where
        spm = gazdaWillemseSPM pa strat

gazdaWillemseSPM :: LiftStrat a => ParityArena -> a -> ProgressMeasure
gazdaWillemseSPM pa@(ArenaPA graph _ _) strat = spmWithin pa mt (Set.fromList $ vertices graph) (zeroMeasure pa) strat
    where
        mt = smallRange pa

spmWithin :: LiftStrat a => ParityArena -> Set Progress -> Set Int -> ProgressMeasure -> a -> ProgressMeasure
spmWithin pa mt w spm strat | null w = spm
                         | otherwise = let ((spm',strat',a),b) = (innerLoop spm initStrat initV) 
                         in if b then spm' else spmWithin pa mt (w Set.\\ a) spm' strat'
    where
        (initV, initStrat) = nextV pa strat
        innerLoop spm' strat' Nothing = breakCond spm' strat'
        innerLoop spm' strat' (Just _) | all (\x -> spm' x /= Pr Nothing) w = innerLoop newSPM newStrat' newV
                                       | otherwise = breakCond spm' strat'
            where
                newSPM = (\x -> if spm' x < lift pa mt spm' x Even x then lift pa mt spm' x Even x else spm' x)
                newStrat = Set.foldl' (\s x -> lifted pa s x) strat' w
                (newV,newStrat') = nextV pa newStrat
        breakCond spm' strat' = case find (\x -> spm' x == Pr Nothing) w of 
                                      Nothing -> ((spm',strat', Set.empty), True) -- the returned set is a dummy
                                      Just v  -> (beforeForAll1 pa mt w spm' strat' v, False)

beforeForAll1 :: LiftStrat a => ParityArena -> Set Progress -> Set Int -> ProgressMeasure -> a -> Int 
                 -> (ProgressMeasure, a, Set Int)
beforeForAll1 pa@(ArenaPA _ pri _) mt w spm strat v = (spmNew3, strat, a)
    where
        k = pri v
        -- sigma is for computing winning strategies, not of interest for now
        --sigma = foldl' (\s x -> if pri x > pri s then x else s) minBound (Set.intersection (successors pa v) (Set.fromList $ toList w'))
        res1 = guardedAttractors pa Even w (Set.singleton v)
        spmNew1 = \x -> if x `Set.member` (Set.delete v res1) then Pr Nothing else spm x
        irr1 = guardedAttractors pa Odd w (Set.filter (\x -> pri x < k) w)
        rem1 = w Set.\\ (res1 <> irr1)
        spmNew2 = spmWithin pa mt rem1 spmNew1 (SubS (rem1,strat)) 
        dom = res1 <> (Set.filter (\x -> spmNew2 x == Pr Nothing) res1)
        a = guardedAttractors pa Even w dom
        outerA = a Set.\\ dom 
        spmNew3 = \x -> if x `Set.member` outerA then Pr Nothing else spmNew2 x
        
-- End section gazdaWillemse

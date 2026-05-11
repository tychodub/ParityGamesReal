{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Explorer (Explorer(..), explore, deadlocks, deadlockPresent, tarjan, tarjanNontrivial) where
import Data.Set
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Debug.Trace (trace)

class Explorer (f :: Type) where
    type State f :: Type
    initStates :: f -> Set (State f)
    successors :: f -> State f -> Set (State f)

explore :: (Explorer f, Ord (State f)) => f -> Set (State f)
explore p = exploreIter p v v
  where
    v = initStates p
     
exploreIter :: (Explorer f, Ord (State f)) => f -> Set (State f) -> Set (State f) -> Set (State f)
exploreIter p q v | Data.Set.null q = v
                  | otherwise = exploreIter p q' v'
    where
        x = head $ toList q
        t = successors p x \\ v
        q' = Data.Set.delete x (q <> t)
        v' = v <> t

deadlocks :: (Explorer f, Ord (State f)) => f -> Set (State f)
deadlocks p = deadlocksIter p v v empty
  where
    v = initStates p
     
deadlocksIter :: (Explorer f, Ord (State f)) => f -> Set (State f) -> Set (State f) -> Set (State f) -> Set (State f)
deadlocksIter p q v dead | Data.Set.null q = dead
                         | otherwise = deadlocksIter p q' v' newDead
    where
        x = head $ toList q
        t' = successors p x
        t =  t' \\ v
        q' = Data.Set.delete x (q <> t)
        v' = v <> t
        newDead = if Data.Set.null t' then Data.Set.insert x dead else dead

-- | specialized version of `null . deadlocks` for performance gains through short-circuiting
deadlockPresent :: (Explorer f, Ord (State f)) => f -> Bool
deadlockPresent p = deadlockPresentIter p v v 
  where
    v = initStates p

deadlockPresentIter :: (Explorer f, Ord (State f)) => f -> Set (State f) -> Set (State f) -> Bool
deadlockPresentIter p q v | Data.Set.null q = False
                          | otherwise = Data.Set.null t' || deadlockPresentIter p q' v'
    where
        x = head $ toList q
        t' = successors p x
        t = t' \\ v
        q' = Data.Set.delete x (q <> t)
        v' = v <> t

tarjan :: (Explorer f, Ord (State f)) => f -> State f -> Map (State f) Int
tarjan p s = let (_,res,_,_) = tarjanStep p s 0 [] Map.empty Map.empty in res

tarjanStep :: (Explorer f, Ord (State f)) => f -> State f -> Int -> [State f] -> Map (State f) Int -> Map (State f) Int 
          -> (Map (State f) Int,Map (State f) Int, [State f], Int) 
tarjanStep p s n stack' idxs' mins' = if idxs3 Map.! s == mins3 Map.! s 
                                     then (idxs3, mins3, (dropWhile (/= s) stack3), j)
                                     else (idxs3, mins3, stack3, j)
    where
      idxs = Map.insert s n idxs'
      mins = Map.insert s n mins'
      m = n+1
      stack = s:stack'
      helper1 []     idxs2 mins2 stack2 i = (idxs2, mins2, stack2, i)
      helper1 (t:ts) idxs2 mins2 stack2 i 
                                 | isNothing (idxs2 Map.!? t) = let (a,b,c,i') = (tarjanStep p t i stack idxs2 mins2) in
                                                    helper1 ts a (Map.insertWith min s (b Map.! t) b) c i'
                                 | t `elem` stack = helper1 ts idxs2 (Map.insertWith min s (mins2 Map.! t) mins2) stack2 i
                                 | otherwise = helper1 ts idxs2 mins2 stack2 i
      (idxs3, mins3, stack3, j) = helper1 (toList $ successors p s) idxs mins stack m

tarjanPartition :: (Explorer f, Ord (State f)) => f -> State f -> Map Int (Set (State f))
tarjanPartition p s = Map.foldrWithKey' (\k a s' -> Map.insertWith Set.union a (Set.singleton k) s') Map.empty idxs
    where
      idxs = tarjan p s

tarjanNontrivial :: (Explorer f, Ord (State f)) => f -> State f -> Map Int (Set (State f))
tarjanNontrivial p s = Map.filter (\s' -> length s' > 1) (tarjanPartition p s)

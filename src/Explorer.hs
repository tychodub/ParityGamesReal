{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Explorer (Explorer(..), explore, deadlocks, deadlockPresent) where
import Data.Set
import Data.Kind (Type)

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

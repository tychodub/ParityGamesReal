{-# LANGUAGE TypeFamilies #-}
module DiningPhilosophers where
import Explorer (Explorer (..))
import Data.Set (empty, fromList, Set, singleton, union)
import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)

data Phil = Thinking | Halfway | Eating deriving (Show, Eq, Ord)

newtype LeftForkFirst = LeftForkFirst Int deriving (Show, Eq)

instance Explorer LeftForkFirst where
    type State LeftForkFirst = (Seq Phil, Seq Int)
    initStates (LeftForkFirst n) | n == 0 = empty
                                 | n == 1 = fromList [(Seq.fromList [Thinking],Seq.fromList [1,1])]
                                 | otherwise = fromList [(Seq.fromList $ replicate n Thinking,
                                                          Seq.fromList $ replicate n 1)]
    successors = leftSuccessors

leftSuccessors :: LeftForkFirst -> (Seq Phil, Seq Int) -> Set (Seq Phil, Seq Int)
leftSuccessors (LeftForkFirst c) (phils,forks) = xs
  where
    xs = Seq.foldMapWithIndex philAct phils
    philAct n Thinking = if fromJust (forks!?n) > 0 
                            then Data.Set.singleton (nextPhil Thinking n,decr n) 
                            else Data.Set.empty
    philAct n Halfway = if fromJust (forks!? ((n+1) `mod` c)) > 0
                           then Data.Set.singleton (nextPhil Halfway n,decr ((n+1) `mod` c))
                           else Data.Set.empty 
    philAct n Eating = Data.Set.singleton (nextPhil Eating n, incr n (incr ((n+1) `mod` c) forks))
    decr n = (Seq.update n) (fromJust (forks!?n) - 1) forks
    incr n forks' = (Seq.update n) (fromJust (forks!?n) + 1) forks'
    nextPhil Thinking n = Seq.update n Halfway phils
    nextPhil Halfway n = Seq.update n Eating phils
    nextPhil Eating n = Seq.update n Thinking phils

data PhilArb = ThinkingArb | LeftArb | RightArb | EatingArb deriving (Show, Eq, Ord)

newtype ArbitraryFork = ArbitraryFork Int deriving (Show, Eq)

instance Explorer ArbitraryFork where
    type State ArbitraryFork = (Seq PhilArb, Seq Int)
    initStates (ArbitraryFork n) | n == 0 = empty
                                 | n == 1 = fromList [(Seq.fromList [ThinkingArb],Seq.fromList [1,1])]
                                 | otherwise = fromList [(Seq.fromList $ replicate n ThinkingArb,
                                                          Seq.fromList $ replicate n 1)]
    successors = arbitrarySuccessors

arbitrarySuccessors :: ArbitraryFork -> (Seq PhilArb, Seq Int) -> Set (Seq PhilArb, Seq Int)
arbitrarySuccessors (ArbitraryFork c) (phils, forks) = xs
  where
    xs = Seq.foldMapWithIndex philAct phils
    leftFork n = if fromJust (forks!?n) > 0 
                    then Data.Set.singleton (Seq.update n LeftArb phils, decr n)
                    else Data.Set.empty
    rightFork n = if fromJust (forks!? ((n+1) `mod` c)) > 0
                    then Data.Set.singleton (Seq.update n RightArb phils,decr ((n+1) `mod` c))
                    else Data.Set.empty 
    philAct n ThinkingArb = Data.Set.union (leftFork n) (rightFork n)
    philAct n RightArb = if fromJust (forks!?n) > 0 
                            then Data.Set.singleton (Seq.update n EatingArb phils,decr n) 
                            else Data.Set.empty
    philAct n LeftArb = if fromJust (forks!? ((n+1) `mod` c)) > 0
                           then Data.Set.singleton (Seq.update n EatingArb phils,decr ((n+1) `mod` c))
                           else Data.Set.empty 
    philAct n EatingArb = Data.Set.singleton (Seq.update n ThinkingArb phils, incr n (incr ((n+1) `mod` c) forks))
    decr n = (Seq.update n) (fromJust (forks!?n) - 1) forks
    incr n forks' = (Seq.update n) (fromJust (forks!?n) + 1) forks'

data CrazyFork = CrazyFork {count :: Int, crazy :: Int} deriving (Show, Eq)

instance Explorer CrazyFork where
    type State CrazyFork = (Seq PhilArb, Seq Int)
    initStates (CrazyFork n _) | n == 0 = empty
                               | n == 1 = fromList [(Seq.fromList [ThinkingArb],Seq.fromList [1,1])]
                               | otherwise = fromList [(Seq.fromList $ replicate n ThinkingArb,
                                                        Seq.fromList $ replicate n 1)]
    successors = crazySuccessors

crazySuccessors :: CrazyFork -> (Seq PhilArb, Seq Int) -> Set (Seq PhilArb, Seq Int)
crazySuccessors (CrazyFork c goober) (phils, forks) = xs
  where
    xs = Seq.foldMapWithIndex philAct phils
    leftFork n = if fromJust (forks!?n) > 0 
                    then Data.Set.singleton (Seq.update n LeftArb phils, decr n)
                    else Data.Set.empty
    rightFork n = if fromJust (forks!? ((n+1) `mod` c)) > 0
                    then Data.Set.singleton (Seq.update n RightArb phils,decr ((n+1) `mod` c))
                    else Data.Set.empty 
    philAct n ThinkingArb | n == goober = rightFork n
                          | otherwise   = leftFork n
    philAct n RightArb = if fromJust (forks!?n) > 0 
                            then Data.Set.singleton (Seq.update n EatingArb phils,decr n) 
                            else Data.Set.empty
    philAct n LeftArb = if fromJust (forks!? ((n+1) `mod` c)) > 0
                           then Data.Set.singleton (Seq.update n EatingArb phils,decr ((n+1) `mod` c))
                           else Data.Set.empty 
    philAct n EatingArb = Data.Set.singleton (Seq.update n ThinkingArb phils, incr n (incr ((n+1) `mod` c) forks))
    decr n = (Seq.update n) (fromJust (forks!?n) - 1) forks
    incr n forks' = (Seq.update n) (fromJust (forks!?n) + 1) forks'

module ParityGames.FixedPointSolver where
import ParityGames.ParityArena
import qualified Data.Set as Set
import Data.Set (Set)
import Explorer (Explorer(..))
import Data.Graph (vertices)
import Debug.Trace (traceShow, traceShowId)

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


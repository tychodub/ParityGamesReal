module Main where
import Criterion.Main
import TS
import LTL (parseLTLInt, LTL (..))
import Pipeline (nbaLTLCheck, reducedNBALTLCheck)
import qualified GHC.Arr as Arr
import ParityGames.ParityArena (flatPA, pruneLeafs, ParityArena, Player(..), ParityGame(..))
import ParityGames.Zielonka (zielonkaStrat)
import ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj)
import ParityGames.ProgressMeasures (spmSlides)
import ParityGames.ForcedPath (forcedPathZielonka)
import qualified Data.Set as Set


main :: IO ()
main = do
    ts1Txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\bench\\benchExamples\\ts1"
    ltl1Txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\bench\\benchExamples\\ltl1"
    let ts1 = parseTS ts1Txt
    let ltl1 = parseLTLInt ltl1Txt
    let (TS a b _ d e) = completeTS [0..50] (Set.fromList [0..5]) (\x -> Set.fromList [0..x])
    let ts2 = TS a b (Set.singleton 0) (\s _ -> d s ()) e
    let ltl2 = LTG (LTTerm 0)
    let (TS a' b' _ d' e') = discreteTS [0..50] (Set.fromList [0..5]) (\x -> Set.fromList [0..x])
    let ts3 = TS a' b' (Set.singleton 0) (\s _ -> d' s ()) e'
    let ltl3 = LTG (LTTerm 0)
    let myltlW = LTW (LTTerm (-9)) (LTNot (LTTerm (-9)))
    let myltlU = LTU (LTTerm (-9)) (LTNot (LTTerm (-9)))
    print (nbaLTLCheck ts1 myltlW)
    print (nbaLTLCheck ts1 myltlU)
    -- -9 W !(-9)
    -- -9 U !(-9)
    defaultMain [
        bgroup "bench part 1" ([
        bench "nba pipeline 1" . nf (\(ts,ltl) -> nbaLTLCheck ts ltl),
        bench "reduced nba pipeline 1" . nf (\(ts,ltl) -> reducedNBALTLCheck ts ltl)
        ]<*>[(ts1,ltl1),(ts2,ltl2),(ts3,ltl3)]),
        bgroup "bench part 2" ([
            bench "zielonka" . nf (\pa -> zielonkaStrat pa),
            (\x -> bench "zielonka pruned" $ nf (\pa -> zielonkaStrat pa) (let (la,_,_) = pruneLeafs x in la)),
            bench "zielonka+pruning" . nf (\pa -> zielonkaStrat (let (la,_,_) = pruneLeafs pa in la)),
            bench "fpi" . nf (\pa -> fpi pa),
            bench "fpiFreeze" . nf (\pa -> fpiFreeze pa),
            bench "fpj" . nf (\pa -> fpj pa),
            bench "spm" . nf (\pa -> spmSlides pa Even),
            bench "forced paths zielonka" . nf (\pa -> forcedPathZielonka pa)
        ]<*>[smallGraph1, fmap fromEnum commonSlidePG])
        ]

smallGraph1 :: ParityArena
smallGraph1 = flatPA $ Arr.array (0,30) [(0,[9]),(1,[6]),(2,[27]),(3,[16,4]),(4,[27]),(5,[23]),(6,[30,24]),(7,[15]),(8,[15]),(9,[27]),(10,[16,6]),(11,[27]),(12,[5]),(13,[21]),(14,[26]),(15,[18,20]),(16,[26]),(17,[13,13,30]),(18,[14]),(19,[3]),(20,[26,14]),(21,[17]),(22,[9]),(23,[19]),(24,[0]),(25,[28,18]),(26,[8,1]),(27,[1]),(28,[9]),(29,[12,9]),(30,[10])]

commonSlidePG :: ParityGame Char
commonSlidePG = ArenaPA (Arr.array (0,8) [(0,[1]),(1,[0,5]),(2,[1,6]),(3,[2,4]),(4,[3,8]),
                                          (5,[6]),(6,[7]),(7,[3,8]),(8,[4,7])]) prio owns nodeVisual
    where
      prio 0 = 0
      prio 1 = 2
      prio 2 = 7
      prio 3 = 1
      prio 4 = 5
      prio 5 = 8
      prio 6 = 6
      prio 7 = 2
      prio 8 = 3
      prio n = error ("common slide PA only has 9 nodes, got value: "++show n) 
      owns n = not (n == 1 || n == 3)
      nodeVisual n = ['a'..'i']!!n

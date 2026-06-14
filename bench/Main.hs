module Main where
import Criterion.Main
import TS
import LTL (parseLTLInt)
import Pipeline (nbaLTLCheck, reducedNBALTLCheck)
import qualified GHC.Arr as Arr
import ParityGames.ParityArena (flatPA, pruneLeafs, ParityArena, Player(..))
import ParityGames.Zielonka (zielonkaStrat)
import ParityGames.FixedPointSolver (fpi, fpiFreeze, fpj)
import ParityGames.ProgressMeasures (spmSlides)


main :: IO ()
main = do
    ts1Txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\bench\\benchExamples\\ts1"
    ltl1Txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\bench\\benchExamples\\ltl1"
    let ts1 = parseTS ts1Txt
    let ltl1 = parseLTLInt ltl1Txt
    defaultMain [
        bgroup "bench part 1" [
        bench "nba pipeline 1" $ nf (\ltl -> nbaLTLCheck ts1 ltl) ltl1,
        bench "reduced nba pipeline 1" $ nf (\ltl -> reducedNBALTLCheck ts1 ltl) ltl1
        ],
        bgroup "bench part 2" ([
            bench "zielonka sg1" . nf (\pa -> zielonkaStrat pa),
            (\x -> bench "zielonka pruned sg1" $ nf (\pa -> zielonkaStrat pa) (let (a,_,_) = pruneLeafs x in a)),
            bench "zielonka+pruning sg1" . nf (\pa -> zielonkaStrat (let (a,_,_) = pruneLeafs pa in a)),
            bench "fpi sg1" . nf (\pa -> fpi pa),
            bench "fpiFreeze" . nf (\pa -> fpiFreeze pa),
            bench "fpj" . nf (\pa -> fpj pa),
            bench "spm" . nf (\pa -> spmSlides pa Even)
        ]<*>[smallGraph1])
        ]

smallGraph1 :: ParityArena
smallGraph1 = flatPA $ Arr.array (0,30) [(0,[9]),(1,[6]),(2,[27]),(3,[16,4]),(4,[27]),(5,[23]),(6,[30,24]),(7,[15]),(8,[15]),(9,[27]),(10,[16,6]),(11,[27]),(12,[5]),(13,[21]),(14,[26]),(15,[18,20]),(16,[26]),(17,[13,13,30]),(18,[14]),(19,[3]),(20,[26,14]),(21,[17]),(22,[9]),(23,[19]),(24,[0]),(25,[28,18]),(26,[8,1]),(27,[1]),(28,[9]),(29,[12,9]),(30,[10])]


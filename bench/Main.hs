module Main where
import Criterion.Main
import TS
import LTL (parseLTLInt)
import Pipeline (nbaLTLCheck, reducedNBALTLCheck)


main :: IO ()
main = do
    ts1Txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\bench\\benchExamples\\ts1"
    ltl1Txt <- readFile "C:\\Users\\tycho\\Documents\\Langs\\Haskell\\ParityGames\\bench\\benchExamples\\ltl1"
    let ts1 = parseTS ts1Txt
    let ltl1 = parseLTLInt ltl1Txt
    defaultMain [
        bgroup "bench1" [
        bench "nba pipeline 1" $ whnf (\ltl -> nbaLTLCheck ts1 ltl) ltl1,
        bench "reduced nba pipeline 1" $ whnf (\ltl -> reducedNBALTLCheck ts1 ltl) ltl1
        ]
        ]

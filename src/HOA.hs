module HOA where
import NBA (NBA (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Dot (quotedString, showNoQuotes)
import Explorer (Explorer(successors))
import Data.Foldable (Foldable(foldMap'))
import Data.List (intercalate)
import GNBA (GNBA (..))

class HOA a where
    toHOA :: a -> String -> String

instance (Ord a, Show a) => HOA (NBA a b) where
    toHOA nba@(NBA states initStates _ _ accept) name = header
        where
            header = "HOA: v1\nname: "++show name++"\nStates: "++show (length stateSeq)++startIndxs++"\nacc-name: Buchi"
                           ++ "\nAcceptance: 1 Inf(0)"++"\n--BODY--"++statesBody++"\n--END--"
            stateSeq = Seq.fromList states
            startIndxs = foldMap' (\n -> "\nStart: "++show n) $ Seq.findIndicesL (`Set.member` initStates) stateSeq
            statesBody = Seq.foldlWithIndex (\str n state -> str++"\nState: "++stateToStr n state) "" stateSeq
                where
                    stateToStr n state = "["++show n++"] "++show n++" "++(quotedString (showNoQuotes state))++ifAccept++edgeStr
                        where
                            ifAccept = if state `Set.member` accept then " {0}" else ""
                            edgeList = Seq.findIndicesL (`Set.member` successors nba state) stateSeq
                            edgeStr | null edgeList = ""
                                    | otherwise = "\n  " ++ intercalate (" ") (map show edgeList)

instance (Ord a, Show a) => HOA (GNBA a b) where
    toHOA nba@(GNBA states initStates _ _ accept) name = header
        where
            header = "HOA: v1\nname: "++show name++"\nStates: "++show (length stateSeq)++startIndxs++"\nacc-name: Buchi"
                           ++ "\nAcceptance: "++show (length accept)++setDecl++"\n--BODY--"++statesBody++"\n--END--"
            setDecl = " " ++ foldMap (\n -> "Inf("++show n++") ") [0..length accept-1]
            acceptingSeq = Seq.fromList (Set.toList accept)
            stateSeq = Seq.fromList states
            startIndxs = foldMap' (\n -> "\nStart: "++show n) $ Seq.findIndicesL (`Set.member` initStates) stateSeq
            statesBody = Seq.foldlWithIndex (\str n state -> str++"\nState: "++stateToStr n state) "" stateSeq
                where
                    stateToStr n state = "["++show n++"] "++show n++" "++(quotedString (showNoQuotes state))++ifAccept++edgeStr
                        where
                            ifAccept | null accIndxs = ""
                                     | otherwise = " {"++intercalate " " (map show accIndxs)++"}"
                                where
                                    accIndxs = Seq.findIndicesL (\acceptSet -> state `Set.member` acceptSet) acceptingSeq
                            edgeList = Seq.findIndicesL (`Set.member` successors nba state) stateSeq
                            edgeStr | null edgeList = ""
                                    | otherwise = "\n  " ++ intercalate (" ") (map show edgeList)            

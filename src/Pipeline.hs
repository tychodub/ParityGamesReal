module Pipeline where
import LTL
import TS
import LTLGNBA (fromLTL)
import NBA (nbaFromGnba, tsMul, dfsLasso)
import qualified GNBA

-- this one is bugged and gives incorrect results, TODO find cause and fix
nbaLTLCheck :: (Ord prop, Ord a, Ord b) => TS a b prop -> LTL prop -> Bool
nbaLTLCheck x y = not $ dfsLasso prod
    where
        negy = LTNot y
        gnba = fromLTL negy
        nba  = nbaFromGnba gnba
        prod = tsMul x nba

gnbaLTLCheck :: (Ord prop, Ord a, Ord b, Show a, Show prop) => TS a b prop -> LTL prop -> Bool
gnbaLTLCheck x y = not $ GNBA.gnbaAccepting prod
    where
        negy = LTNot y
        gnba = fromLTL negy
        prod = GNBA.tsMul x gnba

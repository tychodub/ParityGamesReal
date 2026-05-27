{-# LANGUAGE TypeFamilies #-}
module ParityArena where
import Data.Graph (Graph, vertices, edges, graphFromEdges)
import Explorer (Explorer(..))
import qualified Data.Set as Set
import Dot
import Data.Set (Set)
import qualified GHC.Arr as Array
import Debug.Trace (trace)

-- | really just a wrapper for graph, as we assume that the odd/even property determines ownership
data ParityArena = ArenaPA { 
    forgetPA :: Graph, 
    prioPA :: Int -> Int,
    evenOwns :: Int -> Bool 
}

flatPA :: Graph -> ParityArena
flatPA graph = ArenaPA graph id even

instance Show ParityArena where
    show = show . forgetPA

instance Dot ParityArena where
  dotNodes (ArenaPA graph pri _) = Set.fromList $ map (\x -> show x++" [label = "++show (pri x)++"]") (vertices graph)
  dotArrows pa = Set.fromList (map (\(l,r) -> (show l,"\"\"",show r)) $ edges (forgetPA pa))
  dotName _ = "ParityArena"

instance Explorer ParityArena where
    type State ParityArena = Int
    initStates pa = Set.fromList $ vertices $ forgetPA pa
    successors pa n = Set.fromList ((forgetPA pa) Array.! n)

data Player = Even | Odd deriving (Show, Eq, Enum)

attractors :: ParityArena -> Set Int -> Player -> Set Int
attractors pa@(ArenaPA graph _ owns) xs Even | xs == newXs = xs
                                             | otherwise = attractors pa newXs Even
    where
        newXs = xs <> canPlay <> mustPlay
        canPlay = foldMap (\(l,r) -> if r `Set.member` xs && owns l then Set.singleton l else Set.empty) (edges graph)
        mustPlay = Set.fromList $ filter (\x -> all (`Set.member` xs) (successors pa x)) (vertices graph)
attractors pa@(ArenaPA graph _ owns) xs Odd | xs == newXs = xs
                                            | otherwise = attractors pa newXs Odd
    where
        newXs = xs <> canPlay <> mustPlay
        canPlay = foldMap (\(l,r) -> if r `Set.member` xs && not (owns l) then Set.singleton l else Set.empty) (edges graph)
        mustPlay = Set.fromList $ filter (\x -> all (`Set.member` xs) (successors pa x)) (vertices graph)

subGame :: Foldable t => ParityArena -> t Int -> (ParityArena, Int -> Int)
subGame (ArenaPA graph pri owns) s = (ArenaPA a newPri newOwns, \x -> let (y,_,_) = b x in y) 
    where
        vs = filter (\v -> elem v s) (vertices graph)
        es = map (\n -> (n,n,graph Array.! n)) vs
        (a,b,_) = Data.Graph.graphFromEdges es
        newPri = \n -> let (x,_,_) = b n in pri x
        newOwns = \n -> let (x,_,_) = b n in owns x

zielonka :: ParityArena -> (Set Int, Set Int)
zielonka pa = zielonkaVanDijk pa id

-- might this only work for connected graphs
zielonkaVanDijk :: ParityArena -> (Int -> Int) -> (Set Int, Set Int)
zielonkaVanDijk pa@(ArenaPA graph pri _) og | null (vertices graph) = (Set.empty, Set.empty)
                                            | bAttract == complW = trace ("complW: "++show complW) $ if even maxPri 
                                                                  then (w0<>Set.map og uAttract,w1) 
                                                                  else (w0,w1<>Set.map og uAttract)
                                            | otherwise = trace ("complW: "++show complW++"\nB: "++show (Set.map og bAttract)) $ if even maxPri 
                                                                  then (w0',w1'<>Set.map og bAttract) 
                                                                  else (w0'<>Set.map og bAttract,w1')
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = Set.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        uAttract = attractors pa uSet (playerFromInt maxPri) 
        vs' = Set.fromList vs Set.\\ uAttract
        (newGraph,og') = subGame pa vs'
        (w0,w1) = zielonkaVanDijk newGraph (og . og')
        complW = if even maxPri then w1 else w0
        bAttract = attractors pa complW (playerFromIntFlipped maxPri)
        (w0',w1') = uncurry zielonkaVanDijk (fmap (\f -> og . f) $ subGame pa (Set.fromList vs Set.\\ bAttract))

{-# LANGUAGE TypeFamilies #-}
module ParityArena where
import Data.Graph (Graph, vertices, edges, graphFromEdges)
import Explorer (Explorer(..))
import qualified Data.Set as Set
import Dot
import Data.Set (Set)
import qualified GHC.Arr as Array
import Debug.Trace (trace)
import Data.Maybe (fromJust)
import Data.Bifunctor (Bifunctor(bimap))

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
  dotNodes (ArenaPA graph pri _) = Set.fromList $ map (\x -> show x++" [label = \""++show x++", "++show (pri x)++"\"]") (vertices graph)
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

subGame :: Foldable t => ParityArena -> t Int -> (ParityArena, Int -> Int, Int -> Int)
subGame (ArenaPA graph pri owns) s = (ArenaPA a newPri newOwns, \x -> let (y,_,_) = b x in y, fromJust . c) 
    where
        vs = filter (\v -> elem v s) (vertices graph)
        es = map (\n -> (n,n,graph Array.! n)) vs
        (a,b,c) = Data.Graph.graphFromEdges es
        newPri = \n -> let (x,_,_) = b n in pri x
        newOwns = \n -> let (x,_,_) = b n in owns x

zielonka :: ParityArena -> (Set Int, Set Int)
zielonka pa = zielonkaVanDijk pa id id

zielonkaVanDijk :: ParityArena -> (Int -> Int) -> (Int -> Int) -> (Set Int, Set Int)
zielonkaVanDijk pa@(ArenaPA graph pri _) og toCurrent | null (vertices graph) = (Set.empty, Set.empty)
                                                      | bAttract == complW = if even maxPri 
                                                                                then (w0<>Set.map og uAttract,w1) 
                                                                                else (w0,w1<>Set.map og uAttract)
                                                      | otherwise = if even maxPri 
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
        (newGraph,og', toCurrent') = subGame pa vs'
        (w0,w1) = zielonkaVanDijk newGraph (og . og') (toCurrent' . toCurrent)
        complW = if even maxPri then w1 else w0
        bAttract = attractors pa (Set.map toCurrent complW) (playerFromIntFlipped maxPri) -- we give total graph indices for non total graph
        (ng2,og2,tc2) = subGame pa (Set.fromList vs Set.\\ bAttract)
        (w0',w1') = zielonkaVanDijk ng2 (og . og2) (tc2 . toCurrent)

attractorsStrat :: ParityArena -> Set Int -> Player -> Set (Int, Int) -> (Set Int,Set (Int, Int))
attractorsStrat pa@(ArenaPA graph _ owns) xs Even strat | xs == newXs = (xs, strat)
                                                        | otherwise = attractorsStrat pa newXs Even (strat<>newStrat)
    where
        newXs = xs <> attracted
        attracted = foldl' (\play x -> 
            if owns x then 
                (if not (Set.member x xs) && any (`Set.member` xs) (successors pa x) 
                    then Set.insert x play  
                    else play
            ) 
            else (if not (Set.member x xs) && all (`Set.member` xs) (successors pa x) 
                    then Set.insert x play
                    else play
            )) Set.empty (vertices graph)
        newStrat = Set.map (\x -> (x,head $ Set.toList (Set.filter (\y -> Set.member y xs) $ successors pa x))) attracted
attractorsStrat pa@(ArenaPA graph _ owns) xs Odd strat | xs == newXs = (xs, strat)
                                                       | otherwise = attractorsStrat pa newXs Odd newStrat
    where
        newXs = xs <> attracted
        attracted = foldl' (\play x -> 
            if owns x then 
                (if not (Set.member x xs) && any (`Set.member` xs) (successors pa x) 
                    then Set.insert x play  
                    else play
            ) 
            else (if not (Set.member x xs) && all (`Set.member` xs) (successors pa x) 
                    then Set.insert x play
                    else play
            )) Set.empty (vertices graph)
        newStrat = Set.map (\x -> (x,head $ Set.toList (Set.filter (\y -> Set.member y xs) $ successors pa x))) attracted

zielonkaStrat :: ParityArena -> (Set Int, Set Int,  Set (Int, Int), Set (Int, Int))
zielonkaStrat pa = zielonkaVanDijkStrat pa id

zielonkaVanDijkStrat :: ParityArena -> (Int -> Int) -> (Set Int, Set Int, Set (Int, Int), Set (Int, Int))
zielonkaVanDijkStrat pa@(ArenaPA graph pri _) og 
                    | null (vertices graph) = (Set.empty, Set.empty, mempty, mempty)
                    | bAttract == complW = if even maxPri 
                                                then (Set.map og (w0 <> uAttract), Set.map og w1,
                                                      Set.map (bimap og og) (s0 <> sA <> pickedUSet (w0 <> uAttract)),
                                                      Set.map (bimap og og) sB) 
                                                else (Set.map og w0,Set.map og (w1 <> uAttract),
                                                      Set.map (bimap og og) sB, 
                                                      Set.map (bimap og og) (s1 <> sA <> pickedUSet (w1 <> uAttract)))
                    | otherwise = if even maxPri 
                                                then (Set.map og w0',Set.map og (w1' <> bAttract), 
                                                      Set.map (bimap og og) s0', 
                                                      Set.map (bimap og og) (s1' <> sB)) 
                                                else (Set.map og (w0' <> bAttract),Set.map og w1', 
                                                      Set.map (bimap og og) (s0' <> sB), Set.map (bimap og og) s1')
    where
        vs = vertices graph
        maxPri = maximum (map pri vs)
        uSet = Set.fromList $ filter (\x-> pri x==maxPri) vs
        playerFromInt n | even n = Even
                        | otherwise = Odd
        playerFromIntFlipped n | even n = Odd
                               | otherwise = Even
        (uAttract,sA) = attractorsStrat pa uSet (playerFromInt maxPri) Set.empty
        vs' = Set.fromList vs Set.\\ uAttract
        (newGraph,og', _) = subGame pa vs'
        (w0,w1,s0,s1) = zielonkaVanDijkStrat newGraph og'
        complW = if even maxPri then w1 else w0
        complS = if even maxPri then s1 else s0
        (bAttract,sB) = attractorsStrat pa complW (playerFromIntFlipped maxPri) complS -- we give total graph indices for non total graph
        (ng2,og2,_) = subGame pa (Set.fromList vs Set.\\ bAttract)
        (w0',w1',s0',s1') = zielonkaVanDijkStrat ng2 og2
        pickedUSet w = Set.map (\z -> (z, head $ Set.toList $ (successors pa z `Set.intersection` w))) uSet
        -- multiple edges get picked still

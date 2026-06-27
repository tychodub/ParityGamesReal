{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module PetriNet where 
import Data.Map ( (!), Map, mapWithKey, unionWith )
import qualified Data.Map
import qualified Explorer (Explorer (..))
import qualified Data.Set
import Dot (Dot(..), showNoQuotes)
import qualified Data.Set as Set
import Data.Kind (Type)

data Petri a b = Petri {
    places :: [a], 
    transitions :: [b], 
    initial :: Map a Int,
    transInput :: Map b [a],
    transOutput :: Map b [a]
} deriving (Show, Eq, Ord)

instance (Ord a, Ord b) => Semigroup (Petri a b) where
    l <> r = Petri (places l <> places r) 
                   (transitions l <> transitions r) 
                   (initial l <> initial r)
                   (unionWith (<>) (transInput l) (transInput r))
                   (unionWith (<>) (transOutput l) (transOutput r))

instance (Ord a, Ord b) => Monoid (Petri a b) where
    mempty = Petri mempty mempty mempty mempty mempty

type PetriState a = Map a Int

prettyPetriState :: Show a => Map a Int -> String
prettyPetriState m = show $ filter (\(_,n) -> n>0) $ Data.Map.toList m

instance (Ord a, Ord b) => Explorer.Explorer (Petri a b) where
    type State (Petri a _) = Map a Int
    initStates p = Data.Set.fromList [initial p]
    successors p s = Data.Set.fromList $ successors p [] s

instance (Show a, Show b, Ord b) => Dot (Petri a b) where
    dotNodes x = Set.fromList (map (\y -> "\"N" ++ showNoQuotes y++"\"") (places x)) 
                 `Set.union` 
                 Set.fromList (map (\y -> "\"T" ++ showNoQuotes y++"\"") (transitions x))
    dotArrows x = (foldMap (\y -> Set.fromList $ map (\z -> 
                                            ("\"N" ++ showNoQuotes z++"\""
                                            ,"\"\""
                                            ,"\"T" ++ showNoQuotes y++"\"")) (inArr y)) arrows)
                  `Set.union`
                  (foldMap (\y -> Set.fromList $ map (\z -> 
                                            ("\"T" ++ showNoQuotes y++"\""
                                            ,"\"\""
                                            ,"\"N" ++ showNoQuotes z++"\"")) (outArr y)) arrows)
        where
            arrows = transitions x
            inArr y = transInput x ! y
            outArr y = transOutput x ! y
    dotName _ = "petri"

enabled :: (Ord a, Ord b) => Petri a b -> PetriState a -> b -> Bool
enabled p marks t = enabledHelper xs
    where
        xs = transInput p ! t
        enabledHelper ys = all (\y -> (marks ! y) > 0) ys

perform :: (Eq a, Ord b) => Petri a b -> PetriState a -> b -> PetriState a
perform p marking x = newMarking
    where
        ins  = transInput p ! x
        outs = transOutput p ! x
        newMarking = Data.Map.mapWithKey 
                       (\k val -> if k `elem` ins 
                                  then val-1 
                                  else (if k `elem` outs 
                                        then val+1 
                                        else val)) marking

-- v can be left empty if no states are to be excluded
-- note, this successor function also excludes the given state for the sake of the exploreIter algorithm
-- do not use if you want to know when a state may loop
successors :: (Ord a, Ord b) => Petri a b -> [PetriState a] -> PetriState a -> [PetriState a]
successors p v x = foldMap exclusion $ map (perform p x) enabledts
    where
        enabledts = filter (enabled p x) (transitions p)
        exclusion p' = if p' `elem` v || p' == x then [] else [p']

-- | Specialised version of `explore` for Petri nets
explorePetri :: (Ord a, Ord b) => Petri a b -> [Map a Int]
explorePetri petri = exploreIter petri [v] [v]
  where
    v = initial petri
     
exploreIter :: (Ord a, Ord b) => Petri a b -> [PetriState a] -> [PetriState a] -> [PetriState a]
exploreIter _ []     v = v
exploreIter p (x:xs) v = exploreIter p q' v'
    where
        t = successors p v x
        q' = xs <> t
        v' = v <> t

class MarkingProp a b c where
    holdsWithMarking :: Petri b c -> a -> PetriState b -> Bool

instance (MarkingProp a n arc, MarkingProp b n arc) => MarkingProp (Either a b) n arc where
    holdsWithMarking petri (Left prop) state = holdsWithMarking petri prop state
    holdsWithMarking petri (Right prop) state = holdsWithMarking petri prop state

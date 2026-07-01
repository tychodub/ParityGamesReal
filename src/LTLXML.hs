{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LTLXML where
import Text.XML.Light
import LTL
import PetriNet (MarkingProp(..), enabled)
import qualified Data.Map as Map

parseLTLXMLFireability :: String -> [LTL (Either Fireable (Cardinality String))] 
parseLTLXMLFireability s = parseLTLXML parseFireableOrCard s

parseLTLXML :: (Show a) => (Element -> a) -> String -> [LTL a] 
parseLTLXML f s = parsedLTL
    where
        xml = parseXML s
        xmlElems1 = elChildren $ head $ filter (\x -> qName (elName x) == "property-set") $ onlyElems xml
        xmlElems2 = filter (\x -> qName (elName x) == "property") xmlElems1
        formulas = filter (\x -> qName (elName x) == "formula") (xmlElems2 >>= elChildren)
        allPaths = fmap (head . elChildren) formulas
        parsedLTL = fmap (parseLTLXMLLayer f) (fmap (head . elChildren) allPaths)

parseLTLXMLLayer :: (Element -> a) -> Element -> LTL a
parseLTLXMLLayer f e | eName == "until" = LTU (nextLayer l) (nextLayer r)
                     | eName == "reach" = nextLayer (head $ elChildren e) -- before U reach
                     | eName == "before" = nextLayer (head $ elChildren e)
                     | eName == "disjunction" = LTOr (nextLayer l) (nextLayer r)
                     | eName == "conjunction" = LTAnd (nextLayer l) (nextLayer r)
                     | eName == "finally" = LTF (nextLayer child)
                     | eName == "globally" = LTG (nextLayer child)
                     | eName == "next" = LTX (nextLayer child)
                     | eName == "negation" = LTNot (nextLayer child)
                     | otherwise = LTTerm (f e)
    where
        eName = qName (elName e)
        nextLayer = parseLTLXMLLayer f
        (l:r:_) = elChildren e
        child = head $ elChildren e

parseFireableOrCard :: Element -> Either Fireable (Cardinality String)
parseFireableOrCard e | eName == "is-fireable" = Left $ Fireable $ cdData $ head $ onlyText $ elContent $ head (elChildren e)
                      | eName == "integer-le" = Right $ CardLE l r
                      | otherwise = error ("could not parse \""++eName++"\" in LTL parser as fireable or cardinality")
    where
        eName = qName (elName e)
        (l:r:_) = map intOrMark (elChildren e)
        intOrMark e2 | eName2 == "integer-constant" = Left $ read $ cdData $ head $ onlyText $ elContent e2
                     | eName2 == "tokens-count" = Right $ cdData $ head $ onlyText $ elContent $ head $ elChildren e2
                     | otherwise = error ("could not determine whether constant or cardinality: \""++eName2++"\"")
            where
                eName2 = qName (elName e2)

newtype Fireable = Fireable String deriving (Show, Eq, Ord)

data Cardinality a = CardLE (Either Integer a) (Either Integer a) deriving (Show, Eq, Ord)

instance MarkingProp Fireable String String where
    holdsWithMarking petri (Fireable arc) state = enabled petri state arc

instance MarkingProp (Cardinality String) String String where
    holdsWithMarking _ (CardLE l r) state = getCard l <= getCard r
        where
            getCard (Left n) = n
            getCard (Right name) = case toInteger <$> state Map.!? name of
                                        Just n -> n 
                                        Nothing -> 0

showFireCard :: Either Fireable (Cardinality String) -> String
showFireCard (Left (Fireable s)) = "(Fireable "++s++")"
showFireCard (Right (CardLE l r)) = "("++showCard l++" <= "++showCard r++")"
    where
        showCard (Left n) = show n
        showCard (Right s) = s

module LTLXML where
import Text.XML.Light
import LTL

parseLTLXMLFireability :: String -> [LTL String] 
parseLTLXMLFireability s = parseLTLXML (Text.XML.Light.ppContent . head . elContent . head . elChildren) s

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

module PNMLParser where
import Text.XML.Light
import PetriNet (Petri (..))
import Data.Maybe (fromJust)
import qualified Data.Map
import Data.Map (fromListWith)
import Debug.Trace (trace, traceShowId)

parsePNML :: String -> Petri String String
parsePNML s = arcsConvert core arcs
    where
        xmlcontents = parseXML s
        xmlelems = onlyElems xmlcontents
        (core,arcs) = foldMap loweruntilInteresting xmlelems

arcsConvert :: Petri String String -> [(String, String)] -> Petri String String
arcsConvert p arcs = Petri (places p) (transitions p) (initial p) (fromListWith (<>) arcsIn) (fromListWith (<>) arcsOut)
    where
        arcsIn = fmap (\(l,r) -> (r,[l])) $ filter placeIn arcs
        placeIn (_,arc) = arc `elem` transitions p 
        arcsOut = fmap (\(l,r) -> (l,[r])) $ filter placeOut arcs
        placeOut (arc,_) = arc `elem` transitions p

loweruntilInteresting :: Element -> (Petri String String, [(String, String)])
loweruntilInteresting e | elemName == "place" = processPlace e
                        | elemName == "transition" = processTransition e
                        | elemName == "arc" = processArc e
                        | otherwise = foldMap loweruntilInteresting (elChildren e)
    where
        elemName = qName (elName e)

processPlace :: Element -> (Petri String String, [(String, String)])
processPlace e = case filterChildrenName (\m -> qName m == "hlinitialMarking") e of
                      []   -> (Petri [placeId] mempty (Data.Map.fromList [(placeId,0)]) mempty mempty, mempty)
                      [x]  -> if length (elChildren x) == 1 && qName (elName (head $ elChildren x)) == "text"  -- test
                        then (Petri [placeId] mempty (Data.Map.fromList [(placeId,markingVal2 x)]) mempty mempty, mempty)
                        else (Petri [placeId] mempty (Data.Map.fromList [(placeId,markingVal x)]) mempty mempty, mempty)
                      (x:xs) -> error "malformed PNML: did not expect any other child outside of marking"
    where
        markingVal x = case (read . attrVal . head . elAttribs) <$> 
                            filterElement (\m -> qName (elName m) == "numberconstant") x of
            Nothing -> 1
            Just y  -> y :: Int
        placeId = fromJust $ findAttr (unqual "id") e
        markingTxtTag x = head $ elChildren x
        markingVal2 x = read $ traceShowId $ cdData $ head $ onlyText $ elContent $ markingTxtTag x :: Int

processTransition :: Element -> (Petri String String, [(String, String)])
processTransition e = (Petri mempty [transID] mempty mempty mempty, mempty)
    where
        transID = fromJust $ findAttr (unqual "id") e

processArc :: Element -> (Petri String String, [(String, String)])
processArc e = (mempty, [(arcSource,arcTarget)])
    where
        arcSource = fromJust $ findAttr (unqual "source") e
        arcTarget = fromJust $ findAttr (unqual "target") e

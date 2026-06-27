{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module TS where
import Data.Set (Set, toList)
import Data.List (intercalate)
import qualified Data.Set as Set
import Dot (Dot(..), showNoQuotes)
import Explorer (Explorer(..))
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec.Token (GenTokenParser(commaSep))
import Data.Either (fromRight)
import PetriNet (Petri, PetriState, MarkingProp, explorePetri, enabled, perform, holdsWithMarking)
import qualified PetriNet as Petri

data TS a b c = TS {
    tsStates :: Set a,
    tsInitial :: Set a,
    tsTransitions :: Set (a,b,a),
    tsLabels :: a -> Set c
}

instance (Show a, Show b, Show c) => Show (TS a b c) where
    show x = "states:\n"++foldMap 
                (\y -> (show y)++" ("++intercalate ", " (map show $ toList $ tsLabels x y)++")\n") (tsStates x)++
             "\ninitial:\n"++foldMap ((++"\n") . show) (tsInitial x)++
             "\ntransitions:\n"++foldMap ((++"\n") . show) (tsTransitions x)

instance (Eq a, Eq b, Eq c, Ord c) => Eq (TS a b c) where
    l == r = tsStates l == tsStates r && tsInitial l == tsInitial r
            && tsTransitions l == tsTransitions r && Set.map (tsLabels l) (tsStates l) == Set.map (tsLabels r) (tsStates r)

instance (Show a, Show b, Show c) => Dot (TS a b c) where
    dotNodes x = Set.map (\y -> "\""++showNoQuotes y++"\" [label = "++"\""++showNoQuotes y++dotLabels y++"\""++"]") (tsStates x)
        where
            dotLabels y = foldMap (\z -> "\n"++showNoQuotes z) (tsLabels x y)
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) (tsTransitions x)
    dotName _ = "TS"

instance Ord a => Explorer (TS a b c) where
    type State (TS a _ _) = a
    initStates t = tsInitial t
    successors t s = Set.map (\(_,_,r) -> r) $ Set.filter (\(l,_,_) -> l==s) (tsTransitions t)

languageDef :: Token.GenLanguageDef String u Identity
languageDef =
    emptyDef
        { Token.identStart      = letter
        , Token.identLetter     = alphaNum <|> char '_'
        , Token.reservedOpNames = []
        , Token.reservedNames   = ["true","false","X","U","W","F","G","R","M"]
        , Token.caseSensitive   = True
        }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace p = spaces *> p <* spaces

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

integer :: Parser Integer
integer = ignoreWhitespace $ Token.integer lexer

comma :: Parser String
comma = Token.comma lexer

tsParser :: Parser (TS Integer Integer Integer)
tsParser = (\(a,b) c d -> TS a c d b) <$> statesParser <*> initialParser <*> transitionParser <* eof

parseTS :: String -> TS Integer Integer Integer
parseTS txt = fromRight (error $ show parseResult) parseResult
    where
        parseResult = parse tsParser "" txt

statesParser :: Parser (Set Integer, Integer -> Set Integer)
statesParser = string "states:" *> (process <$> many ((,) <$> integer <*> bPar))
    where
        bPar = ignoreWhitespace $ parens (commaSep lexer integer)
        -- insane way of defining this
        process xs = foldMap (\(l,r) -> (Set.singleton l, (\x -> if x==l then Set.fromList r else Set.empty))) xs

initialParser :: Parser (Set Integer)
initialParser = string "initial:" *> (Set.fromList <$> many integer)

transitionParser :: Parser (Set (Integer, Integer, Integer))
transitionParser = string "transitions:" *> (Set.fromList <$> many transition)
    where
        transition = ignoreWhitespace $ parens ((,,) <$> integer <*> (comma *> integer) <*> (comma *> integer))

completeTS :: Ord a => Set a -> Set a -> (a -> Set c) -> TS a () c
completeTS a b c = TS a b transitions c 
    where
        transitions = Set.map (\(l,r) -> (l,(),r)) $ Set.cartesianProduct a a

discreteTS :: Ord a => Set a -> Set a -> (a -> Set c) -> TS a () c
discreteTS a b c = TS a b Set.empty c

fromPetri :: (MarkingProp prop a b, Ord a, Ord b) => 
             Petri a b -> Set prop -> TS (PetriState a) b prop
fromPetri petri props = TS states (initStates petri) transits propEval
    where
        states = Set.fromList (explorePetri petri)
        transits = Set.fromList $ foldMap (\state -> map (\b -> (state,b,perform petri state b)) 
                                        $ (filter (enabled petri state) (Petri.transitions petri))) states
        propEval x = Set.filter (\prop -> holdsWithMarking petri prop x) props

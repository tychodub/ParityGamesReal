{-# LANGUAGE TypeFamilies #-}
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
import PetriNet (Petri, PetriState, MarkingProp, explorePetri, enabled, perform)
import qualified PetriNet as Petri
import qualified Data.Foldable as Foldable

data TS a b c = TS {
    tsStates :: [a],
    tsInitial :: Set a,
    tsTransitions :: a -> [(b,a)],
    tsLabels :: a -> Set c
}

instance (Show a, Show b, Show c, Ord b, Ord a) => Show (TS a b c) where
    show x = "states:\n"++foldMap 
                (\y -> (show y)++" ("++intercalate ", " (map show $ toList $ tsLabels x y)++")\n") (tsStates x)++
             "\ninitial:\n"++foldMap ((++"\n") . show) (tsInitial x)++
             "\ntransitions:\n"++foldMap ((++"\n") . show) (allTransitionsTS x)

instance (Eq a, Eq b, Eq c, Ord c, Ord b, Ord a) => Eq (TS a b c) where
    l == r = tsStates l == tsStates r && tsInitial l == tsInitial r
            && allTransitionsTS l == allTransitionsTS r && map (tsLabels l) (tsStates l) == map (tsLabels r) (tsStates r)

instance (Show a, Show b, Show c, Ord b, Ord a) => Dot (TS a b c) where
    dotNodes x = Set.fromList $ map (\y -> "\""++showNoQuotes y++"\" [label = "++"\""++showNoQuotes y++dotLabels y++"\""++"]") (tsStates x)
        where
            dotLabels y = foldMap (\z -> "\n"++showNoQuotes z) (tsLabels x y)
    dotArrows x = Set.map (\(a,b,c) -> ("\""++showNoQuotes a++"\"", 
                                        "\""++showNoQuotes b++"\"", 
                                        "\""++showNoQuotes c++"\"")) 
                                    (allTransitionsTS x)
    dotName _ = "TS"

instance Ord a => Explorer (TS a b c) where
    type State (TS a _ _) = a
    initStates t = tsInitial t
    successors t s = Set.fromList $ map (\(_,r) -> r) (tsTransitions t s)

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

statesParser :: Parser ([Integer], Integer -> Set Integer)
statesParser = string "states:" *> (process <$> many ((,) <$> integer <*> bPar))
    where
        bPar = ignoreWhitespace $ parens (commaSep lexer integer)
        -- insane way of defining this
        process xs = foldMap (\(l,r) -> (pure l, (\x -> if x==l then Set.fromList r else Set.empty))) xs

initialParser :: Parser (Set Integer)
initialParser = string "initial:" *> (Set.fromList <$> many integer)

transitionParser :: Parser (Integer -> [(Integer, Integer)])
transitionParser = string "transitions:" *> ((mconcat . map (\(a,b,c) -> (\x -> if x == a then [(b,c)] else []))) 
                                            <$> many transition)
    where
        transition = ignoreWhitespace $ parens ((,,) <$> integer <*> (comma *> integer) <*> (comma *> integer))

completeTS :: (Ord a, Foldable t) => t a -> Set a -> (a -> Set c) -> TS a () c
completeTS a b c = TS (Foldable.toList a) b transitions c 
    where
        transitions _ = map (\x -> ((),x)) (Foldable.toList a)

discreteTS :: (Ord a, Foldable t) => t a -> Set a -> (a -> Set c) -> TS a () c
discreteTS a b c = TS (Foldable.toList a) b (const []) c

allTransitionsTS :: (Ord b, Ord a) => TS a b c -> Set (a,b,a)
allTransitionsTS ts = Set.fromList $ (tsStates ts) >>= (\l -> map (\(m,r) -> (l,m,r)) (tsTransitions ts l)) 

fromPetri :: (MarkingProp prop, Ord a, Ord b) => Petri a b -> TS (Petri a b, PetriState a) b prop
fromPetri petri = undefined
    where
        -- making this many copies is sus, but required if we want to delay computation of prop
        states = map (\x -> (petri, x)) (explorePetri petri)
        transits = Set.fromList $ foldMap (\(_,state) -> map (\b -> (state,b,perform petri state b)) 
                                        $ (filter (enabled petri state) (Petri.transitions petri))) states

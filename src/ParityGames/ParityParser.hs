module ParityGames.ParityParser where
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (GenTokenParser(commaSep))
import ParityGames.ParityArena
import Data.Functor.Identity (Identity)
import qualified GHC.Arr as Arr
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as IntSet

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

int :: Parser Int
int = fmap fromInteger integer

comma :: Parser String
comma = Token.comma lexer

-- input:
-- parity <num>;
-- <state num> <priority> <owner> <edges (comma seperated)>

parityPrefixParser :: Parser Int
parityPrefixParser = ignoreWhitespace (string "parity ") *> int <* char ';'

parityArenaParser :: Int -> Parser ParityArena
parityArenaParser n = do
    allLines <- allLinesParser
    let (edgeList,priMap,ownSet) = foldl' (\(eL,pM,oS) (nodeId, nodePri, nodeOwn, nodeEdges) -> 
            ((nodeId,nodeEdges):eL,
            Map.insert nodeId nodePri pM,
            if even nodeOwn then IntSet.insert nodeId oS else oS)) ([],Map.empty, IntSet.empty) allLines
    pure (ArenaPA (boundsArr edgeList) (priMap Map.!) (`IntSet.member` ownSet) id)
    where
        boundsArr = Arr.array (0,n-1) 
        lineParser = (\a b c d -> (a,b,c,d)) <$> int <*> int <*> int <*> sepEndBy1 int comma <* char ';'
        allLinesParser = many lineParser

-- output:
-- paritysol <num>;
-- <state num> <winner> <strat if won by owner>

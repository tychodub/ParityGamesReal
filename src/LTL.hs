module LTL where
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set

data LTL prop = LTAnd (LTL prop) (LTL prop) 
              | LTOr (LTL prop) (LTL prop)
              | LTImpl (LTL prop) (LTL prop)
              | LTEqv (LTL prop) (LTL prop)
              | LTU (LTL prop) (LTL prop)
              | LTW (LTL prop) (LTL prop)
              | LTR (LTL prop) (LTL prop)
              | LTM (LTL prop) (LTL prop)
              | LTX (LTL prop)
              | LTF (LTL prop)
              | LTG (LTL prop)
              | LTNot (LTL prop)
              | LTTrue 
              | LTFalse
              | LTTerm prop
              deriving (Eq, Ord)

instance Show prop => Show (LTL prop) where
    show (LTAnd l r)  = "("++show l++" & "++show r++")"
    show (LTOr l r)   = "("++show l++" | "++show r++")"
    show (LTImpl l r) = "("++show l++" -> "++show r++")"
    show (LTEqv l r)  = "("++show l++" <-> "++show r++")"
    show (LTU l r)    = "("++show l++" U "++show r++")"
    show (LTW l r)    = "("++show l++" W "++show r++")"
    show (LTR l r)    = "("++show l++" R "++show r++")"
    show (LTM l r)    = "("++show l++" M "++show r++")"
    show (LTX a)      = "X "++show a
    show (LTF a)      = "F "++show a
    show (LTG a)      = "G "++show a
    show (LTNot a)    = "!"++show a
    show LTTrue       = "true"
    show LTFalse      = "false"
    show (LTTerm p)   = show p

recurse1LTL :: (LTL prop -> LTL prop) ->  LTL prop -> LTL prop 
recurse1LTL f (LTAnd a b) = LTAnd (f a) (f b)
recurse1LTL f (LTOr a b) = LTOr (f a) (f b)
recurse1LTL f (LTImpl a b) = LTImpl (f a) (f b)
recurse1LTL f (LTEqv a b) = LTEqv (f a) (f b)
recurse1LTL f (LTU a b) = LTU (f a) (f b)
recurse1LTL f (LTW a b) = LTW (f a) (f b)
recurse1LTL f (LTR a b) = LTR (f a) (f b)
recurse1LTL f (LTM a b) = LTM (f a) (f b)
recurse1LTL f (LTX a) = LTX (f a)
recurse1LTL f (LTF a) = LTF (f a)
recurse1LTL f (LTG a) = LTG (f a)
recurse1LTL f (LTNot a) = LTNot (f a)
recurse1LTL _ x = x

recurseWithLTL :: (LTL prop -> a) -> (a -> a -> a) -> LTL prop -> a
recurseWithLTL f g (LTAnd a b) = g (f a) (f b)
recurseWithLTL f g (LTOr a b) = g (f a) (f b)
recurseWithLTL f g (LTImpl a b) = g (f a) (f b)
recurseWithLTL f g (LTEqv a b) = g (f a) (f b)
recurseWithLTL f g (LTU a b) = g (f a) (f b)
recurseWithLTL f g (LTW a b) = g (f a) (f b)
recurseWithLTL f g (LTR a b) = g (f a) (f b)
recurseWithLTL f g (LTM a b) = g (f a) (f b)
recurseWithLTL f _ (LTX a) = (f a)
recurseWithLTL f _ (LTF a) = (f a)
recurseWithLTL f _ (LTG a) = (f a)
recurseWithLTL f _ (LTNot a) = (f a)
recurseWithLTL f _ x = f x

languageDef :: Token.GenLanguageDef String u Identity
languageDef =
    emptyDef
        { Token.identStart      = letter
        , Token.identLetter     = alphaNum <|> char '_'
        , Token.reservedOpNames = ["->","&","|","<->","~","!"]
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
ignoreWhitespace = between whitespace whitespace

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parensOr :: Parser (LTL String) -> Parser (LTL String)
parensOr p = try p <|> parens ltlLvl4

ltlTerm :: Parser (LTL String)
ltlTerm = parensOr $ 
          (reserved "true" $> LTTrue) <|>
          (reserved "false" $> LTFalse) <|>
          (fmap LTTerm identifier)

ltlLvl1 :: Parser (LTL String)
ltlLvl1 = parensOr $ 
          try ((reservedOp "~" <|> reservedOp "!") *> fmap LTNot ltlParser) <|>
          try (reserved "X" *> fmap LTX ltlParser) <|>
          ltlTerm

ltlLvl2 :: Parser (LTL String)
ltlLvl2 = parensOr $ 
          try (reserved "F" *> fmap LTF ltlLvl2) <|>
          try (reserved "G" *> fmap LTG ltlLvl2) <|>
          ltlLvl1

ltlLvl3 :: Parser (LTL String)
ltlLvl3 = parensOr $ try (chainl1 ltlLvl2 (reservedOp "&" *> pure LTAnd
                                <|> reservedOp "|" *> pure LTOr
                                <|> reservedOp "->" *> pure LTImpl
                                <|> reservedOp "<->" *> pure LTEqv))
            <|> ltlLvl2

ltlLvl4 :: Parser (LTL String)
ltlLvl4 = parensOr $ try (chainr1 ltlLvl3 (reserved "U" *> pure LTU
                                  <|> reserved "W" *> pure LTW
                                  <|> reserved "R" *> pure LTR
                                  <|> reserved "M" *> pure LTM))

ltlParser :: Parser (LTL String)
ltlParser = parensOr ltlLvl4

-- | extremely naive normalization algorithm
-- | normalizing should be idempotent (TODO: make a QuickCheck test for this)
normalize :: LTL prop -> LTL prop
normalize (LTNot LTTrue) = LTFalse
normalize (LTNot LTFalse) = LTTrue
normalize (LTNot (LTNot a)) = a -- assumes LTL is a classical logic
normalize (LTNot (LTG a)) = LTF (normalize $ LTNot a)
normalize (LTNot (LTF a)) = LTG (normalize $ LTNot a)
normalize (LTNot (LTX a)) = LTX (normalize $ LTNot a)
normalize (LTNot (LTU a b)) = LTR (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTW a b)) = LTAnd (normalize $ LTNot (LTU a b)) (LTF (LTNot a)) -- forlater steps I remove the W and M operators
normalize (LTNot (LTR a b)) = LTU (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTM a b)) = normalize (LTNot $ LTU b (LTAnd a b))
normalize (LTNot (LTAnd a b)) = LTOr (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTOr a b)) = LTAnd (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTImpl a b)) = LTAnd (normalize a) (normalize $ LTNot b)
normalize (LTNot (LTEqv a b)) = LTOr (LTAnd (normalize a) (normalize $ LTNot b)) (LTAnd (normalize $ LTNot a) (normalize b))
normalize x = recurse1LTL normalize x

closure :: Ord prop => LTL prop -> Set (LTL prop)
closure (LTNot (LTTerm name)) = Set.singleton $ LTTerm name
closure (LTTerm name) = Set.singleton $ LTTerm name
closure LTTrue = Set.singleton LTTrue
closure LTFalse = Set.singleton LTFalse
closure x = Set.union (Set.singleton x) (recurseWithLTL closure Set.union x)

elemLTL :: Ord prop => LTL prop -> Set (LTL prop) -> Bool
elemLTL (LTNot x) s = not (x `Set.member` s)
elemLTL x s = x `Set.member` s

-- | truth set -> 
locallyConsistent :: Ord prop => Set (LTL prop) -> LTL prop -> Bool
locallyConsistent _ LTFalse = False
locallyConsistent _ LTTrue = True
locallyConsistent s (LTAnd l r) = (l `elemLTL` s && r `elemLTL` s) == ((LTAnd l r) `Set.member` s)
locallyConsistent s (LTOr l r) = (l `elemLTL` s || r `elemLTL` s) == ((LTOr l r) `Set.member` s)
locallyConsistent s (LTImpl l r) = (not (l `elemLTL` s) || r `elemLTL` s) == ((LTImpl l r) `Set.member` s)
locallyConsistent s (LTEqv l r) = ((l `elemLTL` s) == (r `elemLTL` s)) == ((LTEqv l r) `Set.member` s)
locallyConsistent s (LTU l r) = if (r `elemLTL` s) 
                                    then (LTU l r `Set.member` s) 
                                    else (if (LTU l r `Set.member` s)
                                              then (l `elemLTL` s)
                                              else True)
locallyConsistent s (LTR l r) = if (l `elemLTL` s && r `elemLTL` s)
                                   then (LTR l r `Set.member` s)
                                   else (if (LTR l r `Set.member` s && not (l `elemLTL` s)) then r `elemLTL` s else True)
locallyConsistent _ _ = True

consistentSubsetsLTL :: Ord prop => Set (LTL prop) -> Set (Set (LTL prop))
consistentSubsetsLTL s = Set.filter (\candidate -> all (locallyConsistent candidate) s) allCandidates
    where
        allCandidates = Set.powerSet s

genStatesLTL :: Ord prop => LTL prop -> Set (Set (LTL prop))
genStatesLTL = consistentSubsetsLTL . closure . normalize

getAtomics :: Ord prop => LTL prop -> Set prop
getAtomics (LTTerm p) = Set.singleton p
getAtomics x = recurseWithLTL getAtomics Set.union x

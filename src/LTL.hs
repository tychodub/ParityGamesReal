{-# LANGUAGE DeriveFunctor #-}
module LTL where
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Dot ( Dot(..), showNoQuotes, quotedString)
import Data.Either (fromRight)

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
              deriving (Eq, Ord, Functor)

instance Applicative LTL where
    pure = LTTerm
    (LTAnd l r) <*> b = LTAnd (l <*> b) (r <*> b)
    (LTOr l r) <*> b = LTOr (l <*> b) (r <*> b)
    (LTImpl l r) <*> b = LTImpl (l <*> b) (r <*> b)
    (LTEqv l r) <*> b = LTEqv (l <*> b) (r <*> b)
    (LTU l r) <*> b = LTU (l <*> b) (r <*> b)
    (LTW l r) <*> b = LTW (l <*> b) (r <*> b)
    (LTR l r) <*> b = LTR (l <*> b) (r <*> b)
    (LTM l r) <*> b = LTM (l <*> b) (r <*> b)
    (LTX a) <*> b = LTX (a <*> b) 
    (LTF a) <*> b = LTF (a <*> b)
    (LTG a) <*> b = LTG (a <*> b)  
    (LTNot a) <*> b = LTNot (a <*> b) 
    LTTrue <*> _ = LTTrue
    LTFalse <*> _ = LTFalse
    (LTTerm x) <*> b = fmap x b

instance Monad LTL where
    (LTAnd l r) >>= f = LTAnd (l >>= f) (r >>= f)
    (LTOr l r) >>= f = LTOr (l >>= f) (r >>= f)
    (LTImpl l r) >>= f = LTImpl (l >>= f) (r >>= f)
    (LTEqv l r) >>= f = LTEqv (l >>= f) (r >>= f)
    (LTU l r) >>= f = LTU (l >>= f) (r >>= f)
    (LTW l r) >>= f = LTW (l >>= f) (r >>= f)
    (LTR l r) >>= f = LTR (l >>= f) (r >>= f)
    (LTM l r) >>= f = LTM (l >>= f) (r >>= f)
    (LTX a) >>= f = LTX (a >>= f)
    (LTF a) >>= f = LTF (a >>= f)
    (LTG a) >>= f = LTG (a >>= f)
    (LTNot a) >>= f = LTNot (a >>= f) 
    LTTrue >>= _ = LTTrue
    LTFalse >>= _ = LTFalse
    (LTTerm x) >>= f = f x

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

instance Show prop => Dot (LTL prop) where
    dotNodes x = Set.map quotedString (dotNodesHelper x)
        where
            dotNodesHelper y = Set.union (Set.singleton $ showNoQuotes y) 
                               (recurseWithLTL (Set.singleton . showNoQuotes) Set.union y)
    dotArrows (LTAnd l r)  = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTAnd l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTAnd l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTOr l r)   = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTOr l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTOr l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTImpl l r) = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTImpl l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTImpl l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTEqv l r)  = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTEqv l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTEqv l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTU l r)    = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTU l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTU l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTW l r)    = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTW l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTW l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTR l r)    = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTR l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTR l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTM l r)    = mconcat [
                                Set.singleton (quotedString (showNoQuotes (LTM l r)),"\"\"",quotedString (showNoQuotes l)),
                                Set.singleton (quotedString (showNoQuotes (LTM l r)),"\"\"",quotedString (showNoQuotes r))
                                ] `Set.union` dotArrows l `Set.union` dotArrows r
    dotArrows (LTX a)      = Set.insert (quotedString (showNoQuotes (LTX a)),"\"\"",quotedString (showNoQuotes a)) $ dotArrows a
    dotArrows (LTF a)      = Set.insert (quotedString (showNoQuotes (LTF a)),"\"\"",quotedString (showNoQuotes a)) $ dotArrows a
    dotArrows (LTG a)      = Set.insert (quotedString (showNoQuotes (LTG a)),"\"\"",quotedString (showNoQuotes a)) $ dotArrows a
    dotArrows (LTNot a)    = Set.insert (quotedString (showNoQuotes (LTNot a)),"\"\"",quotedString (showNoQuotes a)) $ dotArrows a
    dotArrows _       = Set.empty
    dotName = quotedString . showNoQuotes

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
ignoreWhitespace p = spaces *> p <* spaces

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

integer :: Parser Integer
integer = Token.integer lexer

parensOr :: Parser prop ->  Parser (LTL prop) -> Parser (LTL prop)
parensOr p' p = try p <|> parens (ltlLvl4 p')

ltlTerm :: Parser prop -> Parser (LTL prop)
ltlTerm p = parensOr p $ 
          (reserved "true" $> LTTrue) <|>
          (reserved "false" $> LTFalse) <|>
          (fmap LTTerm p)

ltlLvl2 :: Parser prop -> Parser (LTL prop)
ltlLvl2 p = parensOr p $ 
          try (reserved "F" *> fmap LTF (parensOr p (ltlLvl2 p))) <|>
          try (reserved "G" *> fmap LTG (parensOr p (ltlLvl2 p))) <|>
          try ((try (ignoreWhitespace (char '~')) <|> ignoreWhitespace (char '!')) *> fmap LTNot (parensOr p (ltlLvl2 p))) <|>
          try (reserved "X" *> fmap LTX (parensOr p (ltlLvl2 p))) <|>
          ltlTerm p

ltlLvl3 :: Parser prop -> Parser (LTL prop)
ltlLvl3 p = parensOr p $ try (chainl1 (ltlLvl2 p) (reserved "&" *> pure LTAnd
                                <|> reserved "|" *> pure LTOr
                                <|> reserved "->" *> pure LTImpl
                                <|> reserved "<->" *> pure LTEqv))
            <|> (ltlLvl2 p)

ltlLvl4 :: Parser prop -> Parser (LTL prop)
ltlLvl4 p = parensOr p $ try (chainr1 (ltlLvl3 p) (reserved "U" *> pure LTU
                                    <|> reserved "W" *> pure LTW
                                    <|> reserved "R" *> pure LTR
                                    <|> reserved "M" *> pure LTM))

ltlParser :: Parser (LTL String)
ltlParser = parensOr identifier (ltlLvl4 identifier)

ltlParserWith :: Parser prop -> Parser (LTL prop)
ltlParserWith p = parensOr p (ltlLvl4 p)

ltlParserInt :: Parser (LTL Integer)
ltlParserInt = parensOr integer (ltlLvl4 integer)

parseLTL :: String -> Either ParseError (LTL String)
parseLTL s = parse ltlParser "" s

parseLTLInt :: String -> LTL Integer
parseLTLInt txt = fromRight (error $ show parseResult) parseResult
    where
        parseResult = parse ltlParserInt "" txt

simplifyLtl :: LTL prop -> LTL prop
simplifyLtl (LTG (LTG x)) = simplifyLtl (LTG x)
simplifyLtl (LTF (LTF x)) = simplifyLtl (LTF x)
simplifyLtl (LTF (LTG (LTF x))) = simplifyLtl (LTG (LTF x))
simplifyLtl (LTG (LTF (LTG x))) = simplifyLtl (LTF (LTG x))
simplifyLtl (LTF (LTOr a b)) = simplifyLtl (LTOr (LTF a) (LTF b))
simplifyLtl (LTG (LTAnd a b)) = simplifyLtl (LTAnd (LTG a) (LTG b))
simplifyLtl x = recurse1LTL simplifyLtl x

-- | extremely naive normalization algorithm
-- | normalizing should be idempotent (TODO: make a QuickCheck test for this)
normalize :: LTL prop -> LTL prop
normalize (LTNot LTTrue) = LTFalse
normalize (LTNot LTFalse) = LTTrue
normalize (LTNot (LTNot a)) = normalize a -- assumes LTL is a classical logic
normalize (LTNot (LTG a)) = normalize $ LTF (LTNot a)
normalize (LTNot (LTF a)) = normalize $ LTG (LTNot a)
normalize (LTNot (LTX a)) = LTX (normalize $ LTNot a)
normalize (LTNot (LTU a b)) = LTR (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTW a b)) = normalize $ LTM (LTNot a) (LTNot b)
normalize (LTNot (LTR a b)) = LTU (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTM a b)) = normalize $ LTW (LTNot a) (LTNot b)
normalize (LTNot (LTAnd a b)) = LTOr (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTOr a b)) = LTAnd (normalize $ LTNot a) (normalize $ LTNot b)
normalize (LTNot (LTImpl a b)) = LTAnd (normalize a) (normalize $ LTNot b)
normalize (LTNot (LTEqv a b)) = LTOr (LTAnd (normalize a) (normalize $ LTNot b)) (LTAnd (normalize $ LTNot a) (normalize b))
normalize (LTF a) = normalize $ LTU LTTrue a
normalize (LTG a) = normalize $ LTR LTFalse a
normalize (LTW a b) = normalize $ LTR b (LTOr a b)
normalize (LTM a b) = normalize $ LTU b (LTAnd a b)
normalize x = recurse1LTL normalize x

closure :: Ord prop => LTL prop -> Set (LTL prop)
closure (LTNot (LTTerm name)) = Set.singleton $ LTTerm name
closure (LTTerm name) = Set.singleton $ LTTerm name
closure LTTrue = Set.singleton LTTrue
closure LTFalse = Set.singleton LTFalse
closure x = Set.union (Set.singleton x) (recurseWithLTL closure Set.union x)

elemLTL :: Ord prop => LTL prop -> Set (LTL prop) -> Bool
elemLTL LTTrue _ = True 
elemLTL (LTNot x) s = not (x `elemLTL` s)
elemLTL x s = x `Set.member` s

-- | takes a set indicating true propositions and a proposition, 
--   returns whether the proposition is locally consistent with the truth state
locallyConsistent :: Ord prop => Set (LTL prop) -> LTL prop -> Bool
locallyConsistent s LTFalse = not (LTFalse `Set.member` s)
locallyConsistent s LTTrue = LTTrue `Set.member` s
locallyConsistent _ (LTTerm _) = True
locallyConsistent s (LTAnd l r) = (l `elemLTL` s && r `elemLTL` s) == ((LTAnd l r) `Set.member` s)
locallyConsistent s (LTOr l r) = (l `elemLTL` s || r `elemLTL` s) == ((LTOr l r) `Set.member` s)
locallyConsistent s (LTImpl l r) = (not (l `elemLTL` s) || r `elemLTL` s) == ((LTImpl l r) `Set.member` s)
locallyConsistent s (LTEqv l r) = ((l `elemLTL` s) == (r `elemLTL` s)) == ((LTEqv l r) `Set.member` s)
locallyConsistent s (LTU l r) = (if (r `elemLTL` s) 
                                    then (LTU l r `Set.member` s) 
                                    else True)
                                    &&
                                (if (LTU l r `Set.member` s) && not (r `elemLTL` s)
                                              then (l `elemLTL` s)
                                              else True)
locallyConsistent s (LTR l r) = (if (l `elemLTL` s && r `elemLTL` s)
                                   then (LTR l r `Set.member` s)
                                   else True)
                                   &&
                                (if LTR l r `Set.member` s
                                    then r `elemLTL` s 
                                    else True)
locallyConsistent _ (LTX _) = True
locallyConsistent _ (LTNot _) = True
locallyConsistent _ (LTW _ _) = error "local consistency check not defined for W"
locallyConsistent _ (LTM _ _) = error "local consistency check not defined for M"
locallyConsistent _ (LTF _)   = error "local consistency check not defined for F"
locallyConsistent _ (LTG _)   = error "local consistency check not defined for G"

consistentSubsetsLTL :: Ord prop => Set (LTL prop) -> Set (Set (LTL prop))
consistentSubsetsLTL s = Set.filter (\candidate -> all (locallyConsistent candidate) s) allCandidates
    where
        allCandidates = Set.powerSet s

genStatesLTL :: Ord prop => LTL prop -> Set (Set (LTL prop))
genStatesLTL = consistentSubsetsLTL . closure . normalize

getAtomics :: Ord prop => LTL prop -> Set prop
getAtomics (LTTerm p) = Set.singleton p
getAtomics LTTrue = Set.empty
getAtomics LTFalse = Set.empty
getAtomics x = recurseWithLTL getAtomics Set.union x

module SExpr where

import Text.Parsec (Parsec, (<|>), (<?>), many, many1, char, try, 
  parse, sepBy, choice, between)
import Text.Parsec.Token (integer, float, whiteSpace, stringLiteral, 
  makeTokenParser)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Language (haskell)

import Data.Either (fromRight)

-- Type representing simple S-Expressions consisting of integers,
-- quoted strings, symbols (i.e. anything that's not a quoted string or an integer)
-- and nested lists of S-Expressions
data SExpr
  = Num Integer
  | Str String
  | Sym String
  | List [SExpr]
  deriving (Eq, Show)

type Parser = Parsec String ()

-- S-Expression parser
sExpr :: Parser SExpr
sExpr = tExpr
  where
    tExpr = between ws ws (tList <|> tAtom)
    ws = whiteSpace haskell
    tAtom = 
      (try (Num <$> integer haskell)) <|>
      (Str <$> stringLiteral haskell) <|>
      (Sym <$> many1 (noneOf "()\"\t\n\r "))
    tList = List <$> between (char '(') (char ')') (many tExpr)

-- Tries to parse an S-Expression from a string. Returns `Nothing` if parsing fails
parseSExpr :: String -> Maybe SExpr
parseSExpr s = case (parse sExpr "" s) of
  Left _ -> Nothing
  Right res -> Just res
    

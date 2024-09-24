module Parser (parseSExpr, parseSExprs, Parser, parseInt, parseBool, parseSymbol, parseList) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import AST (SExpr(..))

type Parser = Parsec Void String

-- Space consumer, skips whitespace and comments
sc :: Parser ()
sc = L.space space1 empty empty

-- Helper for parsing lexemes (i.e., tokens followed by spaces)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Helper for parsing specific symbols (e.g., parentheses)
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parser for integers, including negative integers
parseInt :: Parser SExpr
parseInt = SInt <$> (fmap negate (char '-' *> (read <$> lexeme (some digitChar)))
             <|> read <$> lexeme (some digitChar))

-- Parser for boolean values (#t and #f)
parseBool :: Parser SExpr
parseBool = (SBool True <$ lexeme (string "#t"))
        <|> (SBool False <$ lexeme (string "#f"))

-- Parser for symbols (e.g., variables, operators, and function names)
parseSymbol :: Parser SExpr
parseSymbol = SSymbol <$> lexeme (some (letterChar <|> oneOf ("+-*<=>?" :: [Char])))

-- Parser for list expressions (Lisp-style lists)
parseList :: Parser SExpr
parseList = SList <$> (symbol "(" *> sepBy parseSExpr sc <* symbol ")")

-- Main parser for S-expressions
parseSExpr :: Parser SExpr
parseSExpr = lexeme (parseSymbol <|> parseInt <|> parseBool <|> parseList)

{-
parseSExpr :: Parser SExpr
parseSExpr = lexeme (parseSymbol <|> parseInt <|> parseBool <|> parseList) <|> return (SList [])
-}

-- Parser for multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = sepEndBy parseSExpr sc

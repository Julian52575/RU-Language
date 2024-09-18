module Parser (parseSExpr, parseSExprs, Parser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import AST (SExpr(..))

type Parser = Parsec Void String

-- Parser for integers
parseInt :: Parser SExpr
parseInt = do
    n <- some digitChar
    return $ SInt (read n)

-- Parser for boolean values (#t and #f)
parseBool :: Parser SExpr
parseBool = do
    b <- choice [string "#t", string "#f"]
    return $ if b == "#t"
             then SBool True
             else SBool False

-- Parser for symbols (e.g., variables, operators, and function names)
parseSymbol :: Parser SExpr
parseSymbol = do
    s <- some (letterChar <|> oneOf "+-*/<=>")  -- Allow symbols and comparison operators
    return $ SSymbol s

-- Parser for list expressions (Lisp-style lists)
parseList :: Parser SExpr
parseList = do
    _ <- char '('              -- Match opening parenthesis
    exprs <- sepBy parseSExpr space1  -- Parse multiple sub-expressions
    _ <- char ')'              -- Match closing parenthesis
    return $ SList exprs       -- Return a list of parsed expressions

-- Main parser for S-expressions
parseSExpr :: Parser SExpr
parseSExpr = try parseBool
         <|> try parseInt
         <|> try parseSymbol
         <|> parseList  -- Handle all expressions inside parentheses

-- Parser for multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = sepEndBy parseSExpr space1

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


-- Parser for symbols (e.g., variables and function names)
parseSymbol :: Parser SExpr
parseSymbol = do
    s <- some (letterChar <|> oneOf "+-*/")
    return $ SSymbol s


-- Parser for list expressions (Lisp-style lists)
parseList :: Parser SExpr
parseList = do
    _ <- char '('
    exprs <- sepBy parseSExpr space1
    _ <- char ')'
    return $ SList exprs
    

-- Main parser for S-expressions
parseSExpr :: Parser SExpr
parseSExpr = try parseBool
         <|> try parseInt
         <|> try parseSymbol
         <|> parseList


-- Parser for multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = sepEndBy parseSExpr space1

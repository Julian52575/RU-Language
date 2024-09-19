module Parser (parseSExpr, parseSExprs, Parser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import AST (SExpr(..))

type Parser = Parsec Void String

-- Helper to consume any leading spaces
sc :: Parser ()
sc = skipMany (char ' ' <|> char '\t' <|> char '\n')  -- Ignore spaces, tabs, and newlines

-- Parser for integers, including negative integers
parseInt :: Parser SExpr
parseInt = do
    sign <- optional (char '-')  -- Optionally match a negative sign
    digits <- some digitChar     -- Match one or more digits
    let number = read digits     -- Convert the matched digits to a number
    return $ SInt (if sign == Just '-' then -number else number)  -- Apply the negative sign if present

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
    s <- some (letterChar <|> oneOf "+-*<=>?")  -- Allow symbols and comparison operators
    return $ SSymbol s

-- Parser for list expressions (Lisp-style lists)
parseList :: Parser SExpr
parseList = do
    _ <- char '('  -- Match opening parenthesis
    sc  -- Consume any spaces after the opening parenthesis
    exprs <- sepBy parseSExpr sc  -- Parse multiple sub-expressions, separated by spaces
    sc  -- Consume any spaces before the closing parenthesis
    _ <- char ')'  -- Match closing parenthesis
    return $ SList exprs  -- Return a list of parsed expressions

-- Main parser for S-expressions
parseSExpr :: Parser SExpr
parseSExpr = sc *> (try parseBool
                 <|> try parseInt
                 <|> try parseSymbol
                 <|> parseList) <* sc  -- Handle all expressions inside parentheses and spaces

-- Parser for multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = sepEndBy parseSExpr sc  -- Handle multiple S-expressions, ignoring spaces

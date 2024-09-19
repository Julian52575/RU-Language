module Parser (parseSExpr, parseSExprs, Parser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import AST (SExpr(..))

type Parser = Parsec Void String

-- Use `space` from Megaparsec to handle spaces, tabs, and newlines
sc :: Parser ()
sc = space

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
    b <- choice [string "#t", string "#f"]  -- Match either "#t" or "#f"
    return $ if b == "#t"                   -- Return SBool based on matched string
             then SBool True
             else SBool False

-- Parser for symbols (e.g., variables, operators, and function names)
parseSymbol :: Parser SExpr
parseSymbol = SSymbol <$> some (letterChar <|> oneOf "+-*<=>?")  -- Match symbols and operators

-- Parser for list expressions (Lisp-style lists)
parseList :: Parser SExpr
parseList = do
    _ <- char '('          -- Match opening parenthesis
    sc                     -- Consume any spaces after the opening parenthesis
    exprs <- sepBy parseSExpr sc  -- Parse multiple sub-expressions, separated by spaces
    sc                     -- Consume any spaces before the closing parenthesis
    _ <- char ')'          -- Match closing parenthesis
    return $ SList exprs    -- Return a list of parsed expressions

-- Main parser for S-expressions
parseSExpr :: Parser SExpr
parseSExpr = sc *> (parseSymbol  -- Order parsers with simpler/frequent cases first
                 <|> parseInt
                 <|> parseBool
                 <|> parseList) <* sc  -- Handle all expressions with proper whitespace management

-- Parser for multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = sepEndBy parseSExpr sc  -- Handle multiple S-expressions, ignoring spaces

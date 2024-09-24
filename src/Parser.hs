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
parseInt = do
    sign <- optional (char '-')  -- Optionally match a negative sign
    digits <- lexeme (some digitChar)  -- Match one or more digits, ensuring spaces are handled
    let number = read digits  -- Convert the matched digits to a number
    return $ SInt (if sign == Just '-' then -number else number)  -- Apply the negative sign if present

-- Parser for boolean values (#t and #f)
parseBool :: Parser SExpr
parseBool = lexeme $ do
    b <- choice [string "#t", string "#f"]  -- Match either "#t" or "#f"
    return $ if b == "#t"                   -- Return SBool based on matched string
             then SBool True
             else SBool False

-- Parser for symbols (e.g., variables, operators, and function names)
parseSymbol :: Parser SExpr
parseSymbol = lexeme $ SSymbol <$> some (letterChar <|> oneOf ("+-*<=>?" :: [Char]))  -- Match symbols and operators

-- Parser for list expressions (Lisp-style lists)
parseList :: Parser SExpr
parseList = do
    _ <- symbol "("        -- Match opening parenthesis, handling spaces automatically
    exprs <- sepBy parseSExpr sc  -- Parse multiple sub-expressions, separated by spaces
    _ <- symbol ")"        -- Match closing parenthesis, handling spaces automatically
    return $ SList exprs    -- Return a list of parsed expressions

-- Main parser for S-expressions
parseSExpr :: Parser SExpr
parseSExpr = lexeme $ parseSymbol  -- Prioritize simpler/frequent cases first
                 <|> parseInt
                 <|> parseBool
                 <|> parseList

-- Parser for multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = sepEndBy parseSExpr sc  -- Handle multiple S-expressions, ignoring spaces

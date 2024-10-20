module Parser.Utils (Parser, sc, lexeme, sym, identifier) where

import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

-- Parser for handling whitespace
sc :: Parser ()
sc = L.space space1 empty empty

-- Lexeme parser (parses a token and ignores trailing spaces)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser (for specific keywords or symbols, e.g., "int", "void", etc.)
sym :: String -> Parser String
sym = L.symbol sc

-- Identifier parser (parses unknown types, i.e., user-defined or undefined types)
identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

module Parser.Pattern (Pattern(..), patternExpr) where

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Parser.Utils (Parser, sym, lexeme, identifier, sc)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Functor

-- Definition of the different patterns in the AST
data Pattern
    = PatLitInt Int                     -- Literal integer pattern
    | PatLitBool Bool                   -- Literal boolean pattern (true/false)
    | PatLitString String               -- Literal string pattern
    | PatWildcard                       -- Wildcard pattern (_)
    | PatOr [Pattern]                   -- OR pattern (e.g., 1 | 2)
    | PatRange Pattern Pattern                  -- Exclusive range (e.g., 1..10)
    | PatRangeInclusive Pattern Pattern         -- Inclusive range (e.g., 1..=10)
    | PatTuple [Pattern]                -- Tuple pattern (e.g., (x, y))
    | PatArray [Pattern]                -- Array pattern (e.g., [x, y, z])
    | PatVar String                     -- Variable pattern (e.g., x)
    deriving (Show, Eq)

-- Operator table for OR patterns (|), modified to combine all patterns into a single PatOr
operatorTable :: [[Operator Parser Pattern]]
operatorTable =
    [ [InfixL (sym "|" Data.Functor.$> combinePatOr)]  -- Left-associative OR operator
    ]

-- Helper function to combine OR patterns into a single PatOr
combinePatOr :: Pattern -> Pattern -> Pattern
combinePatOr (PatOr ps1) (PatOr ps2) = PatOr (ps1 ++ ps2)  -- Combine two PatOr lists
combinePatOr (PatOr ps) p = PatOr (ps ++ [p])              -- Add a pattern to PatOr
combinePatOr p (PatOr ps) = PatOr (p : ps)                 -- Prepend a pattern to PatOr
combinePatOr p1 p2 = PatOr [p1, p2]                       -- Create a new PatOr

-- Top-level pattern parser using makeExprParser for operator handling
patternExpr :: Parser Pattern
patternExpr = makeExprParser basicPatternExpr operatorTable

-- Basic pattern expressions (without OR operator)
basicPatternExpr :: Parser Pattern
basicPatternExpr = choice
    [ try patRangeParser
    , try patTuple
    , try patArray
    , PatLitInt <$> lexeme (L.signed sc L.decimal)
    , PatLitBool True <$ sym "true"
    , PatLitBool False <$ sym "false"
    , PatLitString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
    , PatWildcard <$ sym "_"
    , PatVar <$> identifier
    ]

-- General parser for range patterns (both exclusive and inclusive)
patRangeParser :: Parser Pattern
patRangeParser = do
    start <- lexeme patVarOrInt
    rangeType <- choice [PatRangeInclusive <$ sym "..=", PatRange <$ sym ".."]
    end <- lexeme patVarOrInt
    return $ rangeType start end

-- Parser to handle either a variable or an integer in ranges
patVarOrInt :: Parser Pattern
patVarOrInt = (PatLitInt <$> L.decimal) <|> (PatVar <$> identifier)

-- Parser for tuple patterns (e.g., (x, y, z))
patTuple :: Parser Pattern
patTuple = PatTuple <$> between (sym "(") (sym ")") (patternExpr `sepBy1` sym ",")

-- Parser for array patterns (e.g., [x, y, z])
patArray :: Parser Pattern
patArray = PatArray <$> between (sym "[") (sym "]") (patternExpr `sepBy` sym "," <|> pure [])

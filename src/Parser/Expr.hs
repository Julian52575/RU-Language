module Parser.Expr 
    ( Expr(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..), expr, parseExpr ) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad.Combinators.Expr
import Parser.Utils (sc, lexeme, sym, identifier)
import Text.Megaparsec.Char


type Parser = Parsec Void String

-- Definition of the Abstract Syntax Tree (AST)
data Expr
    = LitInt Int                -- Integer literal
    | LitString String          -- String literal
    | LitBool Bool              -- Boolean literal
    | Var String                -- Variable
    | FuncCall String [Expr]    -- Function call with a list of arguments
    | BinArith ArithOp Expr Expr-- Binary arithmetic operation
    | BinComp CompOp Expr Expr  -- Binary comparison operation
    | BinLogic LogicOp Expr Expr-- Binary logical operation
    | UnaryLogic UnaryOp Expr   -- Unary logical operation
    | Ternary Expr Expr Expr    -- Ternary expression (condition ? expr1 : expr2)
    | LitArray [Expr]           -- Array literal
    | LitTuple [Expr]           -- Tuple literal
    | Assign Expr Expr          -- Assignment (expr = expr)
    | ArrayIndex Expr Expr      -- Array indexing (expr[expr])
    deriving (Show, Eq)

data ArithOp = Add | Subtract | Multiply | Divide | Modulo deriving (Show, Eq)
data CompOp = Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual deriving (Show, Eq)
data LogicOp = And | Or deriving (Show, Eq)
data UnaryOp = Not deriving (Show, Eq)

-- Parses literals: handles integers, strings, booleans, arrays, and tuples
litExpr :: Parser Expr
litExpr = choice
    [ LitInt <$> lexeme (L.signed sc L.decimal)
    , LitString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
    , LitBool True <$ sym "true"
    , LitBool False <$ sym "false"
    , LitArray <$> between (sym "[") (sym "]") (expr `sepBy` sym ",")
    , parseTupleOrParenExpr
    ]

-- Parses tuples or parenthesized expressions
parseTupleOrParenExpr :: Parser Expr
parseTupleOrParenExpr = between (sym "(") (sym ")") $ do
    firstExpr <- expr
    restExpr <- optional (sym "," *> expr `sepBy1` sym ",")
    return $ maybe firstExpr (LitTuple . (firstExpr :)) restExpr

-- Parses function calls
funcCallExpr :: Parser Expr
funcCallExpr = do
    funcName <- identifier
    args <- parens (expr `sepBy` sym ",")
    return $ FuncCall funcName args

-- Parses array indexing expressions
arrayIndexExpr :: Parser Expr
arrayIndexExpr = do
    arr <- Var <$> identifier
    indexes <- some (between (sym "[") (sym "]") expr)
    return $ foldl ArrayIndex arr indexes

-- Parses comparison operators
binaryCompOp :: Parser CompOp
binaryCompOp = choice
    [ Equal <$ sym "==", NotEqual <$ sym "!="
    , LessEqual <$ sym "<=", LessThan <$ sym "<"
    , GreaterEqual <$ sym ">=", GreaterThan <$ sym ">"
    ]

-- Parses logical operators (AND, OR)
binaryLogicOp :: Parser LogicOp
binaryLogicOp = choice
    [ And <$ (sym "&&" <|> sym "and")
    , Or  <$ (sym "||" <|> sym "or")
    ]

-- Parses unary logical operators (NOT)
unaryLogicOp :: Parser UnaryOp
unaryLogicOp = Not <$ (sym "!" <|> sym "not")

-- Operator table for arithmetic, comparison, logical operations, and assignment (right-associative)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [Prefix (UnaryLogic <$> unaryLogicOp)]
    , [InfixL (BinArith Multiply <$ sym "*"), InfixL (BinArith Divide <$ sym "/"), InfixL (BinArith Modulo <$ sym "%")]
    , [InfixL (BinArith Add <$ sym "+"), InfixL (BinArith Subtract <$ sym "-")]
    , [InfixL (BinComp <$> binaryCompOp)]
    , [InfixL (BinLogic <$> binaryLogicOp)]
    , [InfixR (Assign <$ sym "=")]
    ]

-- Basic terms of expressions: function calls, array indexing, literals, or variables
term :: Parser Expr
term = choice
    [ try funcCallExpr
    , try arrayIndexExpr
    , litExpr
    , Var <$> identifier
    ]

-- Parses expressions with the operator table
expr :: Parser Expr
expr = makeExprParser term operatorTable >>= parseTernaryExpr

-- Parses ternary expressions (condition ? expr1 : expr2)
parseTernaryExpr :: Expr -> Parser Expr
parseTernaryExpr condition = option condition $ do
    thenExpr <- sym "?" *> expr
    elseExpr <- sym ":" *> expr
    return $ Ternary condition thenExpr elseExpr

-- Parses expressions inside parentheses
parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")

-- Main function to parse a complete expression
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = runParser (sc *> expr <* eof) ""

module Parser.Expr 
    ( Expr(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..), expr, parseExpr ) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad.Combinators.Expr
import Parser.Utils (Parser, sc, lexeme, sym, identifier)
import Text.Megaparsec.Char
import Parser.Type (pType, Type(..))
import Parser.AST (Expr(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..))

-- Parses a block of expressions
blockExpr :: Parser Expr -> Parser Expr
blockExpr exprParser = do
    _ <- sym "{"
    exprs <- exprParser `sepBy` sym ";"
    _ <- sym "}"
    return $ BlockExpr exprs

-- Parses a function parameter (name: type)
param :: Parser (String, Type)
param = do
    paramName <- identifier
    _ <- sym ":"
    paramType <- pType
    return (paramName, paramType)

-- Parses a list of parameters for a function
paramsParser :: Parser [(String, Type)]
paramsParser = between (sym "(") (sym ")") (param `sepBy` sym ",")

-- Parses anonymous functions (lambda)
lambdaExpr :: Parser Expr
lambdaExpr = do
    params <- paramsParser
    _ <- sym "->"
    returnType <- pType
    body <- blockExpr expr
    return $ LambdaExpr params returnType body

-- Parses literals: integers, strings, booleans, arrays, and tuples
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
    func <- try arrayIndexExpr <|> Var <$> identifier  -- Function can be a variable or an array index expression
    args <- parens (expr `sepBy` sym ",")
    return $ FuncCall func args

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

-- Defines the operator precedence table for arithmetic, comparison, logic, and assignment
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [Prefix (UnaryLogic <$> unaryLogicOp)]
    , [InfixL (BinArith Multiply <$ sym "*"), InfixL (BinArith Divide <$ sym "/"), InfixL (BinArith Modulo <$ sym "%")]
    , [InfixL (BinArith Add <$ sym "+"), InfixL (BinArith Subtract <$ sym "-")]
    , [InfixL (BinComp <$> binaryCompOp)]
    , [InfixL (BinLogic <$> binaryLogicOp)]
    , [InfixR (Assign <$ sym "=")]
    ]

-- Defines basic terms of expressions: function calls, array indexing, literals, lambdas, or variables
term :: Parser Expr
term = choice
    [ try funcCallExpr
    , try arrayIndexExpr
    , try lambdaExpr
    , litExpr
    , Var <$> identifier
    ]

-- Parses expressions with the operator precedence table
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

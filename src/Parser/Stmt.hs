module Parser.Stmt (Stmt(..), RangeType(..), parseStmt) where

import Text.Megaparsec
import Parser.Utils (Parser, sc, sym, identifier)
import Parser.Expr (Expr(..), expr)
import Parser.Type (pType, Type(..))
import Data.Maybe (fromMaybe)
import Data.Void


data RangeType = Exclusive | Inclusive
    deriving (Show, Eq)

-- AST definition for statements
data Stmt
    = LetStmt String (Maybe Type) Expr         -- Variable declaration (let)
    | ReturnStmt (Maybe Expr)                  -- Return statement with an optional expression
    | BlockStmt [Stmt]                         -- Block of statements
    | ExprStmt Expr                            -- A simple expression as a statement
    | IfStmt Expr Stmt (Maybe Stmt)            -- If statement with optional else branch
    | ForRangeStmt String Expr Expr RangeType (Maybe Expr) [Stmt]  -- For loop with range
    | ForClassicStmt (Maybe Stmt) Expr (Maybe Expr) [Stmt]         -- Classic for loop
    | WhileStmt Expr [Stmt]                    -- While loop
    | DoWhileStmt [Stmt] Expr                  -- Do-while loop
    | BreakStmt                                -- Break statement
    | ContinueStmt                             -- Continue statement
    deriving (Show, Eq)

-- Utility function for parsing braces { }
inBraces :: Parser a -> Parser a
inBraces = between (sym "{") (sym "}")

-- Utility function for parsing parentheses ( )
inParens :: Parser a -> Parser a
inParens = between (sym "(") (sym ")")

-- Parses a let declaration
letDecl :: Parser Stmt
letDecl = do
    _ <- sym "let"
    varName <- identifier
    varType <- optional (sym ":" *> pType)
    _ <- sym "="
    exprVal <- expr
    _ <- sym ";"
    return $ LetStmt varName varType exprVal

-- Parses a let declaration without a semicolon, for use inside for loops
letDeclNoSemi :: Parser Stmt
letDeclNoSemi = do
    _ <- sym "let"
    varName <- identifier
    varType <- optional (sym ":" *> pType)
    _ <- sym "="
    LetStmt varName varType <$> expr

-- Parses a return statement
returnStmt :: Parser Stmt
returnStmt = do
    _ <- sym "return"
    exprVal <- optional expr
    _ <- sym ";"
    return $ ReturnStmt exprVal

-- Parses a block of statements surrounded by { }
blockStmt :: Parser Stmt
blockStmt = BlockStmt <$> inBraces (many stmt)

-- Parses a single expression as a statement
exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr <* optional (sym ";")

-- Parses an if statement with an optional else branch
ifStmt :: Parser Stmt
ifStmt = do
    _ <- sym "if"
    cond <- expr
    thenBranch <- blockStmt
    elseBranch <- optional (sym "else" *> (try ifStmt <|> blockStmt))
    return $ IfStmt cond thenBranch elseBranch

-- Parses a for loop with range (and optional parentheses around the range)
forRangeStmt :: Parser Stmt
forRangeStmt = do
    _ <- sym "for"
    (var, start, rangeType, end, step) <- between (optional (sym "(")) (optional (sym ")")) $ do
        var <- identifier
        _ <- sym "in"
        start <- expr
        rangeType <- (Inclusive <$ sym "..=") <|> (Exclusive <$ sym "..")
        end <- expr
        step <- optional (sym "step" *> expr)
        return (var, start, rangeType, end, step)
    body <- inBraces (many stmt)
    return $ ForRangeStmt var start end rangeType step body

-- Parses a classic for loop (with initialization, condition, and increment, optional parentheses)
forClassicStmt :: Parser Stmt
forClassicStmt = do
    _ <- sym "for"
    (initStmt, condExpr, incrExpr) <- inParens $ do
        initStmt <- optional (try letDeclNoSemi) <* sym ";"
        condExpr <- optional expr <* sym ";"
        incrExpr <- optional expr
        return (initStmt, fromMaybe (LitBool True) condExpr, incrExpr)
    body <- inBraces (many stmt)
    return $ ForClassicStmt initStmt condExpr incrExpr body

-- Parses a while loop
whileStmt :: Parser Stmt
whileStmt = do
    _ <- sym "while"
    cond <- expr
    body <- inBraces (many stmt)
    return $ WhileStmt cond body

-- Parses a do-while loop
doWhileStmt :: Parser Stmt
doWhileStmt = do
    _ <- sym "do"
    body <- inBraces (many stmt)
    _ <- sym "while"
    cond <- expr
    _ <- sym ";"
    return $ DoWhileStmt body cond

-- Parses a break statement
breakStmt :: Parser Stmt
breakStmt = BreakStmt <$ (sym "break" <* sym ";")

-- Parses a continue statement
continueStmt :: Parser Stmt
continueStmt = ContinueStmt <$ (sym "continue" <* sym ";")

-- Parses a complete statement (includes all possible statements)
stmt :: Parser Stmt
stmt = choice
    [ letDecl
    , returnStmt
    , ifStmt
    , try forClassicStmt
    , forRangeStmt
    , whileStmt
    , doWhileStmt
    , breakStmt
    , continueStmt
    , blockStmt
    , exprStmt
    ]

-- Main function to parse a statement from a string input
parseStmt :: String -> Either (ParseErrorBundle String Void) Stmt
parseStmt = runParser (sc *> stmt <* eof) ""

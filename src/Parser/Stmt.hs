module Parser.Stmt (Stmt(..), RangeType(..), parseStmt, blockStmt, stmt) where

import Text.Megaparsec
import Parser.Utils (Parser, sc, sym, identifier)
import Parser.Expr (expr)
import Parser.Type (pType, Type(..))
import Data.Maybe (fromMaybe)
import Data.Void
import Parser.AST (RangeType(..), Stmt(..), Expr(..))
import Parser.Pattern (Pattern, patternExpr)

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
exprStmt = ExprStmt <$> expr <* sym ";"

-- Parses a single expression without requiring a semicolon (used for match branches)
exprStmtWithoutSemi :: Parser Stmt
exprStmtWithoutSemi = ExprStmt <$> expr

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

-- Parses a match statement
matchStmt :: Parser Stmt
matchStmt = do
    _ <- sym "match"
    matchExpr <- expr  -- The expression to match
    _ <- sym "{"       -- Open brace for the match body
    firstBranch <- matchBranch
    otherBranches <- many (sym "," *> matchBranch)
    _ <- sym "}"             -- Close brace for the match body
    return $ MatchStmt matchExpr (firstBranch : otherBranches)

-- Parses a single match branch (pattern => statement or block)
matchBranch :: Parser (Pattern, Stmt)
matchBranch = do
    pat <- patternExpr
    _ <- sym "=>"
    branchStmt <- choice [blockStmt, stmtWithoutSemi]
    return (pat, branchStmt)

-- Parses a statement without requiring a semicolon (for simple cases like return)
stmtWithoutSemi :: Parser Stmt
stmtWithoutSemi = choice
    [ returnStmtWithoutSemi
    , ifStmt
    , try forClassicStmt
    , forRangeStmt
    , whileStmt
    , doWhileStmt
    , matchStmt
    , blockStmt
    , exprStmtWithoutSemi
    ]

-- Parses a return statement without requiring a semicolon
returnStmtWithoutSemi :: Parser Stmt
returnStmtWithoutSemi = do
    _ <- sym "return"
    exprVal <- optional expr
    return $ ReturnStmt exprVal

-- Parses a break statement
breakStmt :: Parser Stmt
breakStmt = BreakStmt <$ (sym "break" <* sym ";")

-- Parses a continue statement
continueStmt :: Parser Stmt
continueStmt = ContinueStmt <$ (sym "continue" <* sym ";")

-- Parses a function parameter (name: type = default_value)
param :: Parser (String, Type, Maybe Expr)
param = do
    paramName <- identifier
    _ <- sym ":"
    paramType <- pType
    defaultValue <- optional (sym "=" *> expr)
    return (paramName, paramType, defaultValue)

-- Parses a list of parameters in a function
paramsParser :: Parser [(String, Type, Maybe Expr)]
paramsParser = between (sym "(") (sym ")") (param `sepBy` sym ",")

-- Parses a function declaration as a statement
funcDeclStmt :: Parser Stmt
funcDeclStmt = do
    _ <- sym "fn"                                -- Keyword 'fn'
    funcName <- identifier                       -- Function name
    params <- paramsParser                       -- Parameters
    _ <- sym "->"                                -- Return type
    returnType <- pType                          -- Return type
    body <- (Just <$> blockStmt) <|> (sym ";" *> pure Nothing)  -- Optional body or semicolon for functions without body
    return $ FuncDeclStmt funcName params returnType body

-- Parses a complete statement (includes all possible statements)
stmt :: Parser Stmt
stmt = choice
    [ funcDeclStmt
    , letDecl
    , returnStmt
    , ifStmt
    , try forRangeStmt
    , forClassicStmt
    , whileStmt
    , doWhileStmt
    , breakStmt
    , continueStmt
    , blockStmt
    , matchStmt
    , exprStmt
    ]

-- Main function to parse a statement from a string input
parseStmt :: String -> Either (ParseErrorBundle String Void) Stmt
parseStmt = runParser (sc *> stmt <* eof) ""

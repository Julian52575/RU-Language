{-# LANGUAGE BangPatterns #-}

module AST (SExpr(..), Ast(..), sexprToAST, symbolToString) where

-- List of reserved keywords
reservedKeywords :: [String]
reservedKeywords = ["define", "lambda", "if", "eq?", "#t", "#f"]

-- List of built-in operators
builtinOperators :: [String]
builtinOperators = ["+", "-", "*", "div", "mod", "<", ">", "<=", ">=", "eq?"]

data SExpr
    = SInt Int
    | SSymbol String
    | SList [SExpr]
    | SBool Bool
    deriving (Eq, Show)

data Ast
    = AstInt Int
    | AstBool Bool
    | AstSym String
    | AstList [Ast]
    | Define String Ast
    | Call String [Ast]
    | Lambda [String] Ast
    | If Ast Ast Ast
    | AstBuiltin String
    | CallLambda Ast [Ast]
    deriving (Eq, Show)

-- General helper for handling lists of SExprs as AST
mapSExprToAST :: [SExpr] -> Either String [Ast]
mapSExprToAST = mapM sexprToAST

-- Function to convert SExpr to AST with error handling using Either
sexprToAST :: SExpr -> Either String Ast
sexprToAST (SInt n) = Right (AstInt n)
sexprToAST (SBool b) = Right (AstBool b)
sexprToAST (SSymbol s) 
    | s `elem` reservedKeywords = Left $ "Error: '" ++ s ++ "' is a reserved keyword and cannot be used as a variable."
    | otherwise = Right (AstSym s)

-- Handle 'define' expressions
sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = 
    Define var <$> sexprToAST expr

-- Handle function definitions like (define (funcname params) body)
sexprToAST (SList [SSymbol "define", SList (SSymbol funcname : params), body]) =
    Lambda <$> mapM symbolToString params <*> sexprToAST body >>= \lambda ->
    Right $ Define funcname lambda

-- Handle 'if' expressions
sexprToAST (SList [SSymbol "if", cond, thenExpr, elseExpr]) = 
    If <$> sexprToAST cond <*> sexprToAST thenExpr <*> sexprToAST elseExpr

-- Handle lambda expressions
sexprToAST (SList [SSymbol "lambda", SList params, body]) =
    Lambda <$> mapM symbolToString params <*> sexprToAST body

-- Handle function calls, including built-in operators
sexprToAST (SList (SSymbol func : args))
    | func `elem` builtinOperators = Call func <$> mapSExprToAST args
    | otherwise = Call func <$> mapSExprToAST args

-- Handle lambda applications
sexprToAST (SList (lambdaExpr : args)) =
    CallLambda <$> sexprToAST lambdaExpr <*> mapSExprToAST args

-- Handle lists of expressions
sexprToAST (SList exprs) = AstList <$> mapSExprToAST exprs

-- Helper function to convert SExpr to a valid string (for parameters)
symbolToString :: SExpr -> Either String String
symbolToString (SSymbol s) = Right s
symbolToString _ = Left "Expected a symbol"


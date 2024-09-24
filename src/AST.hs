module AST (SExpr(..), Ast(..), sexprToAST) where

-- List of reserved keywords (without operators like '+', '-', etc.)
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
    deriving Show

-- Function to convert SExpr to AST with error handling using Either
sexprToAST :: SExpr -> Either String Ast
sexprToAST (SInt n) = Right (AstInt n)
sexprToAST (SBool b) = Right (AstBool b)
sexprToAST (SSymbol s) 
    | s `elem` reservedKeywords = Left $ "Error: '" ++ s ++ "' is a reserved keyword and cannot be used as a variable."
    | otherwise = Right (AstSym s)

-- Handle 'define' expressions
sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = 
    sexprToAST expr >>= \ast ->
    Right $ Define var ast

-- Handle function definitions like (define (funcname params) body)
sexprToAST (SList [SSymbol "define", SList (SSymbol funcname : params), body]) =
    mapM symbolToString params >>= \paramNames ->
    sexprToAST body >>= \bodyAst ->
    Right $ Define funcname (Lambda paramNames bodyAst)

-- Handle 'if' expressions
sexprToAST (SList [SSymbol "if", cond, thenExpr, elseExpr]) = 
    If <$> sexprToAST cond <*> sexprToAST thenExpr <*> sexprToAST elseExpr

-- Handle lambda expressions
sexprToAST (SList [SSymbol "lambda", SList params, body]) =
    mapM symbolToString params >>= \paramNames ->
    sexprToAST body >>= \bodyAst ->
    Right $ Lambda paramNames bodyAst

-- Handle function calls, including built-in operators
sexprToAST (SList (SSymbol func : args)) 
    | func `elem` builtinOperators = Call func <$> mapM sexprToAST args
    | otherwise = Call func <$> mapM sexprToAST args

-- Handle lambda applications
sexprToAST (SList (lambdaExpr : args)) =
    sexprToAST lambdaExpr >>= \funcAst ->
    mapM sexprToAST args >>= \argAsts ->
    Right $ CallLambda funcAst argAsts

-- Handle lists of expressions
sexprToAST (SList exprs) = 
    AstList <$> mapM sexprToAST exprs

-- Helper function to convert SExpr to a valid string (for parameters)
symbolToString :: SExpr -> Either String String
symbolToString (SSymbol s) = Right s
symbolToString _ = Left "Expected a symbol"

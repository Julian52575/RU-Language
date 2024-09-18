module AST (SExpr(..), Ast(..), sexprToAST) where


data SExpr
    = SInt Int
    | SSymbol String
    | SList [SExpr]
    | SBool Bool
    deriving Show


data Ast
    = Define String Ast       -- Variable (or function) definition
    | AstInt Int              -- Integer constant
    | AstBool Bool            -- Boolean constant (e.g., #t or #f)
    | AstSym String           -- Symbol
    | AstList [Ast]           -- List of expressions (can represent Lisp lists)
    | Call String [Ast]       -- Function call, with the function name and arguments
    | If Ast Ast Ast          -- Conditional structure (if CONDITION THEN ELSE)
    | Lambda [String] Ast     -- Lambda function, with a list of parameters and a body
    deriving Show


sexprToAST :: SExpr -> Maybe Ast
-- Integer literals
sexprToAST (SInt n) = Just (AstInt n)

-- Boolean literals
sexprToAST (SBool b) = Just (AstBool b)

-- Symbols (e.g., variables and function names)
sexprToAST (SSymbol s) = Just (AstSym s)

-- Define statement: (define var expr)
sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = do
    astExpr <- sexprToAST expr
    Just (Define var astExpr)

-- Conditional expression: (if cond thenExpr elseExpr)
sexprToAST (SList [SSymbol "if", cond, thenExpr, elseExpr]) = do
    condAst <- sexprToAST cond
    thenAst <- sexprToAST thenExpr
    elseAst <- sexprToAST elseExpr
    Just (If condAst thenAst elseAst)

-- Lambda expression: (lambda (params) body)
sexprToAST (SList [SSymbol "lambda", SList params, body]) = do
    paramNames <- mapM symbolToString params  -- Ensure parameters are symbols
    bodyAst <- sexprToAST body
    Just (Lambda paramNames bodyAst)
  where
    symbolToString (SSymbol s) = Just s
    symbolToString _ = Nothing  -- Invalid if params aren't symbols

-- Function call: (func arg1 arg2 ...)
sexprToAST (SList (SSymbol func : args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call func astArgs)

-- Default case: any other S-expression that doesn't match known patterns
sexprToAST _ = Nothing

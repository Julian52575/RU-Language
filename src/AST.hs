module AST (SExpr(..), Ast(..), sexprToAST) where

data SExpr
    = SInt Int
    | SSymbol String
    | SList [SExpr]
    | SBool Bool
    deriving Show

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
sexprToAST (SSymbol s) = Right (AstSym s)

sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = do
    astExpr <- sexprToAST expr
    Right (Define var astExpr)

sexprToAST (SList [SSymbol "define", SList (SSymbol funcname : params), body]) = do
    paramNames <- mapM symbolToString params
    bodyAst <- sexprToAST body
    Right (Define funcname (Lambda paramNames bodyAst))

sexprToAST (SList [SSymbol "if", cond, thenExpr, elseExpr]) = do
    condAst <- sexprToAST cond
    thenAst <- sexprToAST thenExpr
    elseAst <- sexprToAST elseExpr
    Right (If condAst thenAst elseAst)

sexprToAST (SList [SSymbol "lambda", SList params, body]) = do
    paramNames <- mapM symbolToString params
    bodyAst <- sexprToAST body
    Right (Lambda paramNames bodyAst)

sexprToAST (SList (SSymbol func : args)) = do
    astArgs <- mapM sexprToAST args
    Right (Call func astArgs)

sexprToAST (SList (lambdaExpr : args)) = do
    funcAst <- sexprToAST lambdaExpr
    argAsts <- mapM sexprToAST args
    Right (CallLambda funcAst argAsts)

sexprToAST (SList exprs) = do
    astExprs <- mapM sexprToAST exprs
    Right (AstList astExprs)

symbolToString :: SExpr -> Either String String
symbolToString (SSymbol s) = Right s
symbolToString _ = Left "Expected a symbol"

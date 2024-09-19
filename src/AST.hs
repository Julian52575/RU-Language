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

-- Function to convert SExpr to AST with error handling
sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt n) = Just (AstInt n)
sexprToAST (SBool b) = Just (AstBool b)
sexprToAST (SSymbol s) = Just (AstSym s)

sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = do
    astExpr <- sexprToAST expr
    Just (Define var astExpr)

sexprToAST (SList [SSymbol "define", SList (SSymbol funcname : params), body]) = do
    paramNames <- mapM symbolToString params
    bodyAst <- sexprToAST body
    Just (Define funcname (Lambda paramNames bodyAst))

sexprToAST (SList [SSymbol "if", cond, thenExpr, elseExpr]) = do
    condAst <- sexprToAST cond
    thenAst <- sexprToAST thenExpr
    elseAst <- sexprToAST elseExpr
    Just (If condAst thenAst elseAst)

sexprToAST (SList [SSymbol "lambda", SList params, body]) = do
    paramNames <- mapM symbolToString params
    bodyAst <- sexprToAST body
    Just (Lambda paramNames bodyAst)

sexprToAST (SList (SSymbol func : args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call func astArgs)

sexprToAST (SList (lambdaExpr : args)) = do
    funcAst <- sexprToAST lambdaExpr
    argAsts <- mapM sexprToAST args
    Just (CallLambda funcAst argAsts)

sexprToAST (SList exprs) = do
    astExprs <- mapM sexprToAST exprs
    Just (AstList astExprs)

symbolToString :: SExpr -> Maybe String
symbolToString (SSymbol s) = Just s
symbolToString _ = Nothing

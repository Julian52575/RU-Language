{-# LANGUAGE BangPatterns #-}

module AST (SExpr(..), Ast(..), sexprToAST, symbolToString) where

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

-- Function to convert SExpr to AST with error handling using Either
sexprToAST :: SExpr -> Either String Ast
sexprToAST (SInt n) = Right (AstInt n)
sexprToAST (SBool b) = Right (AstBool b)
sexprToAST (SSymbol s) = Right (AstSym s)

sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = 
    Define var <$> sexprToAST expr

sexprToAST (SList [SSymbol "define", SList (SSymbol funcname : params), body]) = do
    !paramNames <- mapM symbolToString params
    !bodyAst <- sexprToAST body
    return $ Define funcname (Lambda paramNames bodyAst)

sexprToAST (SList [SSymbol "if", cond, thenExpr, elseExpr]) = 
    If <$> sexprToAST cond <*> sexprToAST thenExpr <*> sexprToAST elseExpr

sexprToAST (SList [SSymbol "lambda", SList params, body]) = do
    !paramNames <- mapM symbolToString params
    !bodyAst <- sexprToAST body
    return $ Lambda paramNames bodyAst

sexprToAST (SList (SSymbol func : args)) = 
    Call func <$> mapM sexprToAST args

sexprToAST (SList (lambdaExpr : args)) = do
    !funcAst <- sexprToAST lambdaExpr
    !argAsts <- mapM sexprToAST args
    return $ CallLambda funcAst argAsts

sexprToAST (SList exprs) = 
    AstList <$> mapM sexprToAST exprs

symbolToString :: SExpr -> Either String String
symbolToString (SSymbol s) = Right s
symbolToString _ = Left "Expected a symbol"

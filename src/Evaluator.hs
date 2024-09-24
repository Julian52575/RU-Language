module Evaluator (evalAST, initEnv, evalDefine, Env) where

import AST (Ast(..))
import qualified Data.Map as Map
import Builtin (evalBuiltinFunction)

type Env = Map.Map String Ast

emptyEnv :: Env
emptyEnv = Map.empty

-- Initialiser l'environnement avec les fonctions builtins
initEnv :: Env
initEnv = foldr (uncurry extendEnv) emptyEnv builtinFunctions
  where
    builtinFunctions = 
        [ ("+", AstBuiltin "+")
        , ("-", AstBuiltin "-")
        , ("*", AstBuiltin "*")
        , ("div", AstBuiltin "div")
        , ("mod", AstBuiltin "mod")
        , ("<", AstBuiltin "<")
        , (">", AstBuiltin ">")
        , ("<=", AstBuiltin "<=")
        , (">=", AstBuiltin ">=")
        , ("eq?", AstBuiltin "eq?")
        ]

-- Étendre l'environnement
extendEnv :: String -> Ast -> Env -> Env
extendEnv = Map.insert

-- Rechercher une variable dans l'environnement
lookupEnv :: String -> Env -> Either String Ast
lookupEnv sym env = case Map.lookup sym env of
    Nothing -> Left $ "Error: variable '" ++ sym ++ "' is not bound."
    Just val -> Right val

-- Appliquer une lambda avec gestion d'erreur
applyLambda :: Env -> [String] -> Ast -> [Ast] -> Either String Ast
applyLambda env params body args = 
    if length params /= length args
    then Left "Error: incorrect number of arguments."
    else evalAST (foldr (uncurry extendEnv) env (zip params args)) body

-- Fonction d'évaluation
evalAST :: Env -> Ast -> Either String Ast
evalAST _ (AstInt n) = Right (AstInt n)
evalAST _ (AstBool b) = Right (AstBool b)
evalAST env (AstSym s) = lookupEnv s env

-- Évaluer un appel de fonction
evalAST env (Call func args) = do
    evalArgs <- mapM (evalAST env) args
    case lookupEnv func env of
        Right (AstBuiltin op) -> evalBuiltinFunction op evalArgs
        Right (Lambda params body) -> applyLambda env params body evalArgs
        _ -> Left $ "Function not found: " ++ func

-- Évaluer un appel de lambda anonyme
evalAST env (CallLambda (Lambda params body) args) = do
    evalArgs <- mapM (evalAST env) args
    applyLambda env params body evalArgs

-- Cas où CallLambda ne contient pas de Lambda, retourne une erreur
evalAST _ (CallLambda _ _) = Left "Error: trying to call a non-lambda expression."

-- Évaluer une liste d'expressions (AstList)
evalAST env (AstList exprs) = do
    evalResults <- mapM (evalAST env) exprs
    return $ AstList evalResults

-- Évaluer une fonction lambda
evalAST _ (Lambda params body) = Right (Lambda params body)

-- Évaluer une définition (Define)
evalAST env (Define var expr) = do
    val <- evalAST env expr
    return $ Define var val

-- Évaluer une condition (If)
evalAST env (If condExpr thenExpr elseExpr) = do
    evalCond <- evalAST env condExpr
    case evalCond of
        AstBool True  -> evalAST env thenExpr
        AstBool False -> evalAST env elseExpr
        _ -> Left "Error: condition in 'if' must evaluate to a boolean."

-- Gérer le cas des AstBuiltin directement (par sécurité)
evalAST _ (AstBuiltin _) = Left "Error: built-in function cannot be evaluated directly."


-- Fonction d'évaluation des définitions
{- 
evalDefine :: Ast -> Env -> Either String Env
evalDefine (Define var expr) env = case expr of
    Lambda _ _ -> Right $ extendEnv var expr env
    _ -> case evalAST env expr of
        Right val -> Right $ extendEnv var val env
        Left err  -> Left err
evalDefine _ env = Right env
-}

evalDefine :: Ast -> Env -> Either String Env
evalDefine (Define var expr) env = case evalAST env expr of
    Right val -> Right $ extendEnv var val env
    Left err  -> Left err
evalDefine _ env = Right env

module Evaluator (evalAST, initEnv, evalDefine, Env) where

import AST (Ast(..))
import qualified Data.Map as Map

type Env = Map.Map String Ast

emptyEnv :: Env
emptyEnv = Map.empty

-- Initial environment with built-in functions
initEnv :: Env
initEnv = foldr (uncurry extendEnv) emptyEnv builtinFunctions

-- Builtin functions with function pointers for faster dispatch
builtinFunctions :: [(String, Ast)]
builtinFunctions =
    [ ("+", AstBuiltin "+")
    , ("-", AstBuiltin "-")
    , ("*", AstBuiltin "*")
    , ("div", AstBuiltin "div")
    , ("<", AstBuiltin "<")
    , (">", AstBuiltin ">")
    , ("<=", AstBuiltin "<=")
    , (">=", AstBuiltin ">=")
    , ("eq?", AstBuiltin "eq?")
    ]

extendEnv :: String -> Ast -> Env -> Env
extendEnv = Map.insert

lookupEnv :: String -> Env -> Maybe Ast
lookupEnv = Map.lookup

applyLambda :: Env -> [String] -> Ast -> [Ast] -> Maybe Ast
applyLambda env params body args = do
    if length params /= length args
        then Nothing
        else do
            let newEnv = foldr (uncurry extendEnv) env (zip params args)
            evalAST newEnv body

evalAST :: Env -> Ast -> Maybe Ast
evalAST _ (AstInt n) = Just (AstInt n)
evalAST _ (AstBool b) = Just (AstBool b)
evalAST env (AstSym s) = lookupEnv s env

evalAST env (Call func args) = do
    evalArgs <- mapM (evalAST env) args
    case lookupEnv func env of
        Just (Lambda params body) -> applyLambda env params body evalArgs
        Just (AstBuiltin op) -> evalBuiltinFunction op evalArgs
        _ -> error $ "Function not found: " ++ func

evalAST env (CallLambda (Lambda params body) args) = do
    evalArgs <- mapM (evalAST env) args
    applyLambda env params body evalArgs

evalAST _ (Lambda params body) = Just (Lambda params body)

evalAST env (If condExpr thenExpr elseExpr) = do
    evalCond <- evalAST env condExpr
    case evalCond of
        AstBool True  -> evalAST env thenExpr
        AstBool False -> evalAST env elseExpr
        _ -> Nothing

evalAST _ _ = Nothing


evalBuiltinFunction :: String -> [Ast] -> Maybe Ast
evalBuiltinFunction func args = do
    intArgs <- mapM getInt args
    case (func, intArgs) of
        ("+", _)        -> return $ AstInt (sum intArgs)
        ("*", _)        -> return $ AstInt (product intArgs)
        ("-", (x:xs))   -> return $ AstInt (foldl (-) x xs)
        ("div", (x:xs)) | 0 `elem` xs -> Nothing
                        | otherwise   -> return $ AstInt (foldl div x xs)
        ("<", [x, y])   -> return $ AstBool (x < y)
        (">", [x, y])   -> return $ AstBool (x > y)
        ("<=", [x, y])  -> return $ AstBool (x <= y)
        (">=", [x, y])  -> return $ AstBool (x >= y)
        ("eq?", [x, y]) -> return $ AstBool (x == y)
        _               -> Nothing

getInt :: Ast -> Maybe Int
getInt (AstInt n) = Just n
getInt _ = Nothing

evalDefine :: Ast -> Env -> Env
evalDefine (Define var expr) env =
    case expr of
        Lambda _ _ -> extendEnv var expr env
        _ -> case evalAST env expr of
            Just val -> extendEnv var val env
            Nothing  -> env
evalDefine _ env = env

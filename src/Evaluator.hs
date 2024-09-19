module Evaluator (evalAST, getInt, initEnv, extendEnv, lookupEnv, evalDefine, Env) where

import AST (Ast(..))
import qualified Data.Map as Map

-- Type for the environment: a Map of variable to value
type Env = Map.Map String Ast

-- Empty environment
emptyEnv :: Env
emptyEnv = Map.empty

-- Initialiser l'environnement avec les fonctions builtin
initEnv :: Env
initEnv = foldr (uncurry extendEnv) emptyEnv builtinFunctions

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

-- Extend the environment using Map
extendEnv :: String -> Ast -> Env -> Env
extendEnv = Map.insert

-- Look up a variable in the environment using Map
lookupEnv :: String -> Env -> Maybe Ast
lookupEnv = Map.lookup

-- Helper function to apply a lambda to arguments
applyLambda :: Env -> [String] -> Ast -> [Ast] -> Maybe Ast
applyLambda env params body args = do
    if length params /= length args
        then Nothing  -- Return Nothing if the number of arguments doesn't match
        else do
            let newEnv = foldr (uncurry extendEnv) env (zip params args)
            evalAST newEnv body

-- Evaluate function call or lambda application
evalAST :: Env -> Ast -> Maybe Ast
evalAST env (AstInt n) = Just (AstInt n)  -- Evaluating an integer literal

evalAST env (AstBool b) = Just (AstBool b)  -- Evaluating a boolean literal

evalAST env (AstSym s) = lookupEnv s env  -- Looking up a symbol in the local environment

evalAST env (Call func args) = do
    evalArgs <- mapM (evalAST env) args  -- Evaluate the arguments before calling the function
    case lookupEnv func env of
        Just (Lambda params body) -> applyLambda env params body evalArgs  -- Application of lambda
        Just (AstBuiltin op) -> evalBuiltinFunction op evalArgs  -- Application of built-in functions
        _ -> Nothing  -- Return Nothing if the function is not found

evalAST env (CallLambda (Lambda params body) args) = do
    evalArgs <- mapM (evalAST env) args
    applyLambda env params body evalArgs  -- Application of anonymous lambda

evalAST env (Lambda params body) = Just (Lambda params body)  -- Handling lambda expressions directly

-- If condition evaluation
evalAST env (If condExpr thenExpr elseExpr) = do
    evalCond <- evalAST env condExpr
    case evalCond of
        AstBool True  -> evalAST env thenExpr
        AstBool False -> evalAST env elseExpr
        _ -> Nothing  -- Return Nothing if the condition is not valid

-- Error case: if the AST doesn't match any of the above
evalAST _ _ = Nothing

-- Function to evaluate built-in functions
evalBuiltinFunction :: String -> [Ast] -> Maybe Ast
evalBuiltinFunction func args = do
    intArgs <- mapM getInt args  -- Convertir les arguments en entiers si applicable
    case (func, intArgs) of
        ("+", _)        -> return $ AstInt (sum intArgs)
        ("*", _)        -> return $ AstInt (product intArgs)
        ("-", (x:xs))   -> return $ AstInt (foldl (-) x xs)
        ("div", (x:xs)) | 0 `elem` xs -> Nothing  -- Return Nothing if division by zero
                        | otherwise   -> return $ AstInt (foldl div x xs)
        ("<", [x, y])   -> return $ AstBool (x < y)
        (">", [x, y])   -> return $ AstBool (x > y)
        ("<=", [x, y])  -> return $ AstBool (x <= y)
        (">=", [x, y])  -> return $ AstBool (x >= y)
        ("eq?", [x, y]) -> return $ AstBool (x == y)
        _               -> Nothing  -- Return Nothing if the function is unsupported

-- Function to get an integer from the AST
getInt :: Ast -> Maybe Int
getInt (AstInt n) = Just n
getInt _ = Nothing  -- Return Nothing if the AST is not an integer

-- Handles definitions and adds the variable to the environment
evalDefine :: Ast -> Env -> Env
evalDefine (Define var expr) env =
    case expr of
        Lambda _ _ -> extendEnv var expr env  -- Store the lambda directly
        _ -> case evalAST env expr of
            Just val -> extendEnv var val env
            Nothing  -> env  -- Return the same environment if evaluation failed
evalDefine _ env = env

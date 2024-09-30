{-# LANGUAGE BangPatterns #-}
module Evaluator (evalAST, initEnv, evalDefine, Env, lookupEnv, expectBool, applyLambda, applyFunction) where

import AST (Ast(..))
import qualified Data.Map as Map
import Builtin (evalBuiltinFunction)

type Env = Map.Map String Ast

emptyEnv :: Env
emptyEnv = Map.empty

-- Initialize the environment with built-in functions
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

-- Extend the environment by adding a new binding
extendEnv :: String -> Ast -> Env -> Env
extendEnv = Map.insert

-- Look up a variable in the environment
lookupEnv :: String -> Env -> Either String Ast
lookupEnv sym = maybe (Left $ "Error: variable '" ++ sym ++ "' is not bound.") Right . Map.lookup sym

-- Helper function to expect a boolean value
expectBool :: Ast -> Either String Bool
expectBool (AstBool b) = Right b
expectBool _ = Left "Error: condition in 'if' must evaluate to a boolean."

-- Apply a lambda function with error handling
applyLambda :: Env -> [String] -> Ast -> [Ast] -> Either String Ast
applyLambda env params body args
    | length params /= length args = Left "Error: incorrect number of arguments."
    | otherwise = let !newEnv = Map.union (Map.fromList (zip params args)) env
                  in evalAST newEnv body

-- Helper to apply a function (built-in or lambda)
applyFunction :: Env -> Ast -> [Ast] -> Either String Ast
applyFunction _ (AstBuiltin op) args = evalBuiltinFunction op args
applyFunction env (Lambda params body) args = applyLambda env params body args
applyFunction _ _ _ = Left "Error: trying to call a non-function."

-- Evaluation function
evalAST :: Env -> Ast -> Either String Ast
evalAST _ (AstInt n) = Right (AstInt n)
evalAST _ (AstBool b) = Right (AstBool b)
evalAST env (AstSym s) = lookupEnv s env

-- Evaluate a function call
evalAST env (Call func args) = 
    mapM (evalAST env) args >>= \evalArgs ->
    lookupEnv func env >>= \funcAst ->
    applyFunction env funcAst evalArgs

-- Evaluate an anonymous lambda call
evalAST env (CallLambda (Lambda params body) args) = 
    mapM (evalAST env) args >>= applyLambda env params body

-- Error when trying to call a non-lambda expression as a lambda
evalAST _ (CallLambda _ _) = Left "Error: trying to call a non-lambda expression."

-- Evaluate a list of expressions (AstList)
evalAST env (AstList exprs) = AstList <$> mapM (evalAST env) exprs

-- Evaluate a lambda function
evalAST _ (Lambda params body) = Right (Lambda params body)

-- Evaluate a definition (Define)
evalAST env (Define var expr) = 
    evalAST env expr >>= \val -> Right (Define var val)

-- Evaluate a condition (If)
evalAST env (If condExpr thenExpr elseExpr) = 
    evalAST env condExpr >>= expectBool >>= \condResult ->
    evalAST env (if condResult then thenExpr else elseExpr)

-- Handle the case where an AstBuiltin is evaluated directly (safety check)
evalAST _ (AstBuiltin _) = Left "Error: built-in function cannot be evaluated directly."

-- Evaluation function for definitions
evalDefine :: Ast -> Env -> Either String Env
evalDefine (Define var expr) env = 
    evalAST env expr >>= \val -> Right $ extendEnv var val env
evalDefine _ env = Right env

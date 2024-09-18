module Evaluator (evalAST, getInt, emptyEnv, extendEnv, lookupEnv, evalDefine, Env) where

import AST (Ast(..))
import qualified Data.Map as Map

-- Type for the environment: a Map of variable to value
type Env = Map.Map String Ast

-- Empty environment
emptyEnv :: Env
emptyEnv = Map.empty

-- Extend the environment using Map
extendEnv :: String -> Ast -> Env -> Env
extendEnv = Map.insert

-- Look up a variable in the environment using Map
lookupEnv :: String -> Env -> Maybe Ast
lookupEnv = Map.lookup

-- Evaluation of the AST with the environment
evalAST :: Env -> Ast -> Maybe Ast
-- Evaluating an integer literal
evalAST _ (AstInt n) = Just (AstInt n)

-- Evaluating a boolean literal
evalAST _ (AstBool b) = Just (AstBool b)

-- Looking up a symbol in the environment and evaluating it
evalAST env (AstSym s) = lookupEnv s env

-- Handling function calls with arguments
evalAST env (Call func args) = do
    evalArgs <- mapM (evalAST env) args  -- Evaluate all arguments
    intArgs <- mapM getInt evalArgs      -- Convert them to integers if applicable
    case func of
        "+" -> return $ AstInt (sum intArgs)
        "*" -> return $ AstInt (product intArgs)
        "-" -> return $ AstInt (foldl1 (-) intArgs)
        "/" -> if elem 0 (tail intArgs)  -- Check for division by zero
                then Nothing
                else return $ AstInt (foldl1 div intArgs)
        "<" -> return $ AstBool (intArgs !! 0 < intArgs !! 1)  -- Handle <
        ">" -> return $ AstBool (intArgs !! 0 > intArgs !! 1)  -- Handle >
        "<=" -> return $ AstBool (intArgs !! 0 <= intArgs !! 1) -- Handle <=
        ">=" -> return $ AstBool (intArgs !! 0 >= intArgs !! 1) -- Handle >=
        "=" -> return $ AstBool (intArgs !! 0 == intArgs !! 1)  -- Handle equality
        _   -> Nothing  -- Unsupported function/operator

-- Handling conditional expressions (if condExpr thenExpr elseExpr)
evalAST env (If condExpr thenExpr elseExpr) = do
    evalCond <- evalAST env condExpr        -- Evaluate the condition
    case evalCond of
        AstBool True  -> evalAST env thenExpr  -- If true, evaluate the then branch
        AstBool False -> evalAST env elseExpr  -- If false, evaluate the else branch
        _             -> Nothing  -- Error if condition doesn't evaluate to a boolean

-- Default case: if the AST does not match any of the above
evalAST _ _ = Nothing

-- Function to get an integer from the AST
getInt :: Ast -> Maybe Int
getInt (AstInt n) = Just n
getInt _ = Nothing


-- Handles definitions and adds the variable to the environment
evalDefine :: Ast -> Env -> Env
evalDefine (Define var expr) env =
    -- We store the unevaluated expression in the environment
    extendEnv var expr env
evalDefine _ env = env

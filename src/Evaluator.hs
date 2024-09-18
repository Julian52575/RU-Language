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
evalAST env (AstSym s) = do
    val <- lookupEnv s env  -- Retrieve the associated expression from the environment
    evalAST env val         -- Evaluate it when the symbol is used

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
        _   -> Nothing

-- Evaluating a conditional expression (If)
evalAST env (If condExpr thenExpr elseExpr) = do
    evalCond <- evalAST env condExpr        -- Evaluate the condition
    case evalCond of
        AstBool True  -> evalAST env thenExpr  -- Evaluate the 'then' branch if true
        AstBool False -> evalAST env elseExpr  -- Evaluate the 'else' branch if false
        _             -> Nothing  -- Error if condition does not evaluate to a boolean

-- Evaluating a lambda function (you'll need to define how to handle this)
-- Here, we just return the Lambda as is, but in a real interpreter, you'd handle function calls
evalAST _ lambda@(Lambda _ _) = Just lambda  -- Placeholder for now

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

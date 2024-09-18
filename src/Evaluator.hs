module Evaluator (evalAST, getInt, emptyEnv, extendEnv, lookupEnv, evalDefine, Env) where

import AST (Ast(..))

-- Type pour l'environnement : une liste d'associations (variable, valeur)
type Env = [(String, Ast)]

-- Environnement vide
emptyEnv :: Env
emptyEnv = []

-- Ajouter une nouvelle variable dans l'environnement
extendEnv :: String -> Ast -> Env -> Env
extendEnv var val env = (var, val) : env

-- Rechercher une variable dans l'environnement
lookupEnv :: String -> Env -> Maybe Ast
lookupEnv _ [] = Nothing
lookupEnv var ((v, val):rest)
    | var == v  = Just val
    | otherwise = lookupEnv var rest

-- Évaluation de l'AST avec environnement
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
        "/" -> if elem 0 (tail intArgs)    -- Check for division by zero
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

-- Fonction pour obtenir un entier à partir de l'AST
getInt :: Ast -> Maybe Int
getInt (AstInt n) = Just n
getInt _ = Nothing

-- Gère les définitions et ajoute la variable dans l'environnement
evalDefine :: Ast -> Env -> Env
evalDefine (Define var expr) env =
    -- On stocke l'expression non évaluée dans l'environnement
    extendEnv var expr env
evalDefine _ env = env

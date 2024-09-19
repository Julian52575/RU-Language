module Evaluator (evalAST, getInt, initEnv, extendEnv, lookupEnv, evalDefine, Env) where

import AST (Ast(..))
import qualified Data.Map as Map
import Debug.Trace

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
    , ("/", AstBuiltin "div")
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

-- Evaluation of the AST with the environment
evalAST :: Env -> Ast -> Maybe Ast
-- Evaluating an integer literal
evalAST _ (AstInt n) = trace ("Evaluating integer: " ++ show n) $ Just (AstInt n)

-- Evaluating a boolean literal
evalAST _ (AstBool b) = trace ("Evaluating boolean: " ++ show b) $ Just (AstBool b)

-- Looking up a symbol in the local environment and evaluating it
evalAST env (AstSym s) = 
    trace ("Looking up symbol: " ++ s) $ 
    case lookupEnv s env of
        Just val -> trace ("Found symbol: " ++ s ++ " with value " ++ show val) $ Just val
        Nothing  -> trace ("Symbol not found: " ++ s) Nothing

-- Appels de fonctions avec arguments
evalAST env (Call func args) = do
    -- Évaluer tous les arguments avant d'appeler la fonction
    evalArgs <- mapM (evalAST env) args
    -- Vérifier si la fonction est une Lambda définie par l'utilisateur
    case lookupEnv func env of
        Just (Lambda params body) -> do
            -- Vérifier la correspondance entre les paramètres et les arguments
            if length params /= length evalArgs
               then Nothing  -- Erreur si le nombre d'arguments ne correspond pas
               else do
                   -- Étendre l'environnement local avec les arguments évalués
                   let localEnv = foldr (uncurry extendEnv) env (zip params evalArgs)
                   evalAST localEnv body  -- Évaluer le corps de la fonction dans le nouvel environnement
        -- Si ce n'est pas une Lambda, on appelle une fonction builtin
        _ -> evalBuiltinFunction func evalArgs  -- Sinon, tenter un appel de fonction builtin


-- Expressions conditionnelles (if CONDITION THEN ELSE)
evalAST env (If condExpr thenExpr elseExpr) = do
    evalCond <- evalAST env condExpr
    case evalCond of
        AstBool True -> evalAST env thenExpr
        AstBool False -> evalAST env elseExpr
        AstBuiltin op -> do
            -- Assuming the condition is a comparison operator with two arguments
            -- Example: (if < 1 2)
            evalArgs <- mapM (evalAST env) [thenExpr, elseExpr] -- Evaluate the two arguments
            evalBuiltinFunction op evalArgs  -- Evaluate using the builtin function logic
        _ -> Nothing  -- Error if the condition is not a valid expression


-- Default case: if the AST does not match any of the above
evalAST _ _ = Nothing

-- Fonction pour évaluer les fonctions builtin (+, -, *, /, >, <, etc.)
evalBuiltinFunction :: String -> [Ast] -> Maybe Ast
evalBuiltinFunction func args = do
    -- Convertir les arguments en entiers si applicable
    intArgs <- mapM getInt args
    case func of
        "+"  -> return $ AstInt (sum intArgs)
        "*"  -> return $ AstInt (product intArgs)
        "-"  -> return $ AstInt (foldl1 (-) intArgs)
        "div" -> if elem 0 (tail intArgs)  -- Vérifier la division par zéro
                 then Nothing
                 else return $ AstInt (foldl1 div intArgs)
        "<"  -> return $ AstBool (intArgs !! 0 < intArgs !! 1)
        ">"  -> return $ AstBool (intArgs !! 0 > intArgs !! 1)
        "<=" -> return $ AstBool (intArgs !! 0 <= intArgs !! 1)
        ">=" -> return $ AstBool (intArgs !! 0 >= intArgs !! 1)
        "eq?" -> return $ AstBool (intArgs !! 0 == intArgs !! 1)
        _    -> Nothing  -- Fonction ou opérateur non supporté



-- Function to get an integer from the AST
getInt :: Ast -> Maybe Int
getInt (AstInt n) = Just n
getInt _ = Nothing

-- Handles definitions and adds the variable to the environment
evalDefine :: Ast -> Env -> Env
evalDefine (Define var expr) env =
    trace ("Defining variable: " ++ var ++ " with expression: " ++ show expr ++ " in environment: " ++ show env) $
    -- We store the evaluated expression (or Lambda) in the environment
    case expr of
        Lambda _ _ -> trace ("Storing Lambda for " ++ var) $ extendEnv var expr env  -- Store the lambda directly
        _          -> case evalAST env expr of
                        Just val -> trace ("Storing evaluated result for " ++ var ++ ": " ++ show val ++ " in environment: " ++ show env) $ extendEnv var val env  -- Store the evaluated result
                        Nothing  -> trace ("Error evaluating definition for " ++ var) env
evalDefine _ env = env

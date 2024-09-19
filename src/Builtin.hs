module Builtin (evalBuiltinFunction) where

import AST (Ast(..))

-- Type des fonctions builtins
type BuiltinFunction = [Ast] -> Either String Ast

-- Table des builtins avec des fonctions de première classe
builtinFunctions :: [(String, BuiltinFunction)]
builtinFunctions =
    [ ("+", evalAdd)
    , ("-", evalSub)
    , ("*", evalMul)
    , ("div", evalDiv)
    , ("mod", evalMod)
    , ("<", evalLt)
    , (">", evalGt)
    , ("<=", evalLe)
    , (">=", evalGe)
    , ("eq?", evalEq)
    ]

-- Fonction pour rechercher et exécuter un builtin
evalBuiltinFunction :: String -> [Ast] -> Either String Ast
evalBuiltinFunction func args = case lookup func builtinFunctions of
    Just f  -> f args
    Nothing -> Left $ "Unknown function: " ++ func  -- Renvoie une erreur si la fonction n'existe pas

-- Implémentation des builtins arithmétiques et de comparaison
evalAdd, evalSub, evalMul, evalDiv, evalMod :: BuiltinFunction

-- Addition
evalAdd [] = Left "Addition requires at least one argument"
evalAdd args = do
    intArgs <- mapM getInt args
    return $ AstInt (sum intArgs)

-- Soustraction
evalSub [] = Left "Subtraction requires at least one argument"
evalSub (x:xs) = do
    intX <- getInt x
    intXs <- mapM getInt xs
    return $ AstInt (foldl (-) intX intXs)

-- Multiplication
evalMul [] = Left "Multiplication requires at least one argument"
evalMul args = do
    intArgs <- mapM getInt args
    return $ AstInt (product intArgs)

-- Division
evalDiv [] = Left "Division requires at least two arguments"
evalDiv (x:xs) = do
    intX <- getInt x
    intXs <- mapM getInt xs
    if 0 `elem` intXs
        then Left "Division by zero error"
        else return $ AstInt (foldl div intX intXs)

-- Modulo
evalMod [x, y] = do
    intX <- getInt x
    intY <- getInt y
    if intY == 0
        then Left "Modulo by zero error"
        else return $ AstInt (intX `mod` intY)
evalMod _ = Left "Modulo requires exactly two arguments"

-- Comparaison
evalLt, evalGt, evalLe, evalGe, evalEq :: BuiltinFunction

evalLt [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX < intY)
evalLt _ = Left "Less than requires two arguments"

evalGt [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX > intY)
evalGt _ = Left "Greater than requires two arguments"

evalLe [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX <= intY)
evalLe _ = Left "Less than or equal to requires two arguments"

evalGe [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX >= intY)
evalGe _ = Left "Greater than or equal to requires two arguments"

evalEq [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX == intY)
evalEq _ = Left "Equality check requires two arguments"

-- Fonction auxiliaire pour extraire un entier d'un Ast
getInt :: Ast -> Either String Int
getInt (AstInt n) = Right n
getInt _ = Left "Expected an integer"

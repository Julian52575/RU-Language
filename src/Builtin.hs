{-# LANGUAGE BangPatterns #-}

module Builtin (evalBuiltinFunction, getInt) where
import AST (Ast(..))
import Data.List (foldl')

type BuiltinFunction = [Ast] -> Either String Ast

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

evalBuiltinFunction :: String -> [Ast] -> Either String Ast
evalBuiltinFunction func args = maybe (Left $ "Unknown function: " ++ func) (\f -> f args) (lookup func builtinFunctions)

-- Helper function to extract an integer from an AST node
getInt :: Ast -> Either String Int
getInt (AstInt n) = Right n
getInt _ = Left "Expected an integer"

-- Arithmetic functions
evalAdd, evalSub, evalMul, evalDiv, evalMod :: BuiltinFunction

-- Addition
evalAdd [] = Left "Addition requires at least one argument"
evalAdd args = do
    !intArgs <- mapM getInt args
    return $ AstInt (sum intArgs)

-- Subtraction
evalSub [] = Left "Subtraction requires at least one argument"
evalSub (x:xs) = do
    !intX <- getInt x
    !intXs <- mapM getInt xs
    return $ AstInt (foldl' (-) intX intXs)

-- Multiplication
evalMul [] = Left "Multiplication requires at least one argument"
evalMul args = do
    !intArgs <- mapM getInt args
    return $ AstInt (product intArgs)

-- Division
evalDiv [] = Left "Division requires at least two arguments"
evalDiv (x:xs) = do
    !intX <- getInt x
    !intXs <- mapM getInt xs
    if 0 `elem` intXs
        then Left "Division by zero error"
        else return $ AstInt (foldl' div intX intXs)

-- Modulo
evalMod [x, y] = do
    !intX <- getInt x
    !intY <- getInt y
    if intY == 0
        then Left "Modulo by zero error"
        else return $ AstInt (intX `mod` intY)
evalMod _ = Left "Modulo requires exactly two arguments"

-- Comparison functions
evalLt, evalGt, evalLe, evalGe, evalEq :: BuiltinFunction

-- Less than
evalLt [x, y] = do
    !intX <- getInt x
    !intY <- getInt y
    return $ AstBool (intX < intY)
evalLt _ = Left "Less than requires two arguments"

-- Greater than
evalGt [x, y] = do
    !intX <- getInt x
    !intY <- getInt y
    return $ AstBool (intX > intY)
evalGt _ = Left "Greater than requires two arguments"

-- Less than or equal to
evalLe [x, y] = do
    !intX <- getInt x
    !intY <- getInt y
    return $ AstBool (intX <= intY)
evalLe _ = Left "Less than or equal to requires two arguments"

-- Greater than or equal to
evalGe [x, y] = do
    !intX <- getInt x
    !intY <- getInt y
    return $ AstBool (intX >= intY)
evalGe _ = Left "Greater than or equal to requires two arguments"

-- Equality check
evalEq [x, y] = do
    !intX <- getInt x
    !intY <- getInt y
    return $ AstBool (intX == intY)
evalEq _ = Left "Equality check requires two arguments"

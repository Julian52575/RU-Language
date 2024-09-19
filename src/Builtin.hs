module Builtin (evalBuiltinFunction) where

import AST (Ast(..))

-- Type alias for built-in functions, taking a list of ASTs and returning either an error message or an AST
type BuiltinFunction = [Ast] -> Either String Ast

-- Table of built-in functions with first-class functions
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

-- Function to look up and execute a built-in function
evalBuiltinFunction :: String -> [Ast] -> Either String Ast
evalBuiltinFunction func args = maybe (Left $ "Unknown function: " ++ func) ($ args) (lookup func builtinFunctions)
-- Uses `maybe` for more concise lookup and function execution

-- Arithmetic and comparison built-in function implementations
evalAdd, evalSub, evalMul, evalDiv, evalMod :: BuiltinFunction

-- Addition
evalAdd [] = Left "Addition requires at least one argument"
evalAdd args = AstInt . sum <$> mapM getInt args
-- Uses `<$>` to simplify the function composition and avoid explicit `do` block

-- Subtraction
evalSub [] = Left "Subtraction requires at least one argument"
evalSub (x:xs) = do
    intX <- getInt x
    intXs <- mapM getInt xs
    return $ AstInt (foldl (-) intX intXs)

-- Multiplication
evalMul [] = Left "Multiplication requires at least one argument"
evalMul args = AstInt . product <$> mapM getInt args
-- Similar optimization with `<$>` for readability

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

-- Comparison functions
evalLt, evalGt, evalLe, evalGe, evalEq :: BuiltinFunction

-- Less than comparison
evalLt [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX < intY)
evalLt _ = Left "Less than requires two arguments"

-- Greater than comparison
evalGt [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX > intY)
evalGt _ = Left "Greater than requires two arguments"

-- Less than or equal comparison
evalLe [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX <= intY)
evalLe _ = Left "Less than or equal to requires two arguments"

-- Greater than or equal comparison
evalGe [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX >= intY)
evalGe _ = Left "Greater than or equal to requires two arguments"

-- Equality comparison
evalEq [x, y] = do
    intX <- getInt x
    intY <- getInt y
    return $ AstBool (intX == intY)
evalEq _ = Left "Equality check requires two arguments"

-- Helper function to extract an integer from an AST node
getInt :: Ast -> Either String Int
getInt (AstInt n) = Right n
getInt _ = Left "Expected an integer"

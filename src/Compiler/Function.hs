module Compiler.Function (
    getFunctionTable,
    unsetFuncVar,
    getFunctionIndex
) where

import Parser.AST (Stmt(..))
import Data.Maybe (fromJust)
import Data.List (elemIndex, findIndex)
import Compiler.Type (OpCode(..), Function(..), Compile(..))

getFunctionIndex :: String -> Compile -> Int
getFunctionIndex name compile =
    case findIndex (\f -> fName f == name) (functionTable compile) of
        Just index -> index
        Nothing -> 0

-- unset all the arguments given to a function
unsetFuncVar :: Int -> Int -> [OpCode]
unsetFuncVar argNb start = zipWith OpUnsetArg [start..(start + argNb - 1)] [0..(argNb - 1)]

-- get a list of functions from a list of statements
getFunctionTable :: [Stmt] -> [String] -> [Function]
getFunctionTable (FuncDeclStmt name _ _ _ : xs) strTable = Function (fromJust $ elemIndex name strTable) name Nothing Nothing : getFunctionTable xs strTable
getFunctionTable (_ : xs) strTable = getFunctionTable xs strTable
getFunctionTable [] _ = []

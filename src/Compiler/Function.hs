module Compiler.Function (
    Function(..),
    getFunctionTable,
    unsetFuncVar,
    getFunctionIndex
) where

import Parser.AST (Stmt(..))
import Data.Maybe (fromJust)
import Data.List (elemIndex, find)
import Compiler.Type (OpCode(..), Function(..), Scope(..), Compile(..))

getFunctionIndex :: String -> Compile -> Int
getFunctionIndex name compile =
    let maybeFunction = find (\f -> fName f == name) (functionTable compile)
    in maybe 0 fIndex maybeFunction

-- unset all the arguments given to a function
unsetFuncVar :: Int -> Int -> [OpCode]
unsetFuncVar argNb start = zipWith OpUnsetArg [start..(start + argNb - 1)] [0..(argNb - 1)]

-- get a list of functions from a list of statements
getFunctionTable :: [Stmt] -> [String] -> [Function]
getFunctionTable (FuncDeclStmt name _ _ _ : xs) strTable = Function (fromJust $ elemIndex name strTable) name Nothing Nothing : getFunctionTable xs strTable
getFunctionTable (_ : xs) strTable = getFunctionTable xs strTable
getFunctionTable [] _ = []

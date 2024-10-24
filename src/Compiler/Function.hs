module Compiler.Function (
    Function(..),
    getFunctionTable
) where

import Parser.AST (Stmt(..))
import Data.Maybe (fromJust)
import Data.List (elemIndex)

data Function = Function {
    fIndex :: Int,
    fName :: String,
    fOffset :: Maybe Int,
    fSize :: Maybe Int
} deriving (Show, Eq)

getFunctionTable :: [Stmt] -> [String] -> [Function]
getFunctionTable (FuncDeclStmt name _ _ _ : xs) strTable = Function (fromJust $ elemIndex name strTable) name Nothing Nothing : getFunctionTable xs strTable
getFunctionTable (_ : xs) strTable = getFunctionTable xs strTable
getFunctionTable [] _ = []

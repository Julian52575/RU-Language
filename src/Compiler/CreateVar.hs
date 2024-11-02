module Compiler.CreateVar(
    getCreateVar,
    getFunctionVar,
    getIndexFromStrTable,
) where

import Parser.AST (Stmt(..), Expr(..))
import Parser.Type (Type(..))
import Compiler.Type (Scope(..), OpCode(..))
import Data.List (elemIndex)
import Data.Either
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

-- get the Index of a string in the string table
getIndexFromStrTable :: [String] -> String -> Int
getIndexFromStrTable vars var = fromJust $ elemIndex var vars

-- get the OpCode for creating a variable
getOpCreateVar :: [String] -> Type -> Expr -> Either String OpCode
getOpCreateVar strTable TInt (LitInt int) = Right $ OpCreateVar 0x01 int
getOpCreateVar strTable TInt _ = Right $ OpCreateVar 0x01 0
getOpCreateVar strTable TString (LitString str) = Right $ OpCreateVar 0x02 (getIndexFromStrTable strTable str)
getOpCreateVar strTable TString _ = Right $ OpCreateVar 0x02 0
getOpCreateVar _ _ _ = Left "Type not supported"

-- get a list of OpCodes and variable names for all the variables given as arguments to a function
getFunctionVar :: Stmt -> [(OpCode, String)]
getFunctionVar (FuncDeclStmt _ args _ _) = mapMaybe getArgOpCode args
  where
    getArgOpCode (var, typ, expr) = case getOpCreateVar [] typ (fromMaybe (LitInt 0) expr) of
        Right opCode -> Just (opCode, var)
        Left _ -> Nothing
getFunctionVar _ = []

-- get a list of OpCodes and variable names for all the variables created in the (global scope)/(function scope)
getCreateVar :: [Stmt] -> [String] -> [(OpCode, String)]
getCreateVar [] _ = []

getCreateVar (LetStmt var (Just typ) expr : xs) strTable =
    case getOpCreateVar strTable typ expr of
        Right opCode -> (opCode, var) : getCreateVar xs strTable
        Left _ -> getCreateVar xs strTable

getCreateVar (IfStmt _ b1 (Just b2) : xs) strTable = getCreateVar (b1 : [b2] ++ xs) strTable
getCreateVar (IfStmt _ b1 Nothing : xs) strTable = getCreateVar (b1: xs) strTable

getCreateVar (BlockStmt stmts : xs) strTable = getCreateVar (stmts ++ xs) strTable

getCreateVar (ForRangeStmt _ _ _ _ _ stmts : xs) strTable = getCreateVar (stmts ++ xs) strTable

getCreateVar (ForClassicStmt (Just stmt) _ _ stmts : xs) strTable = getCreateVar (stmt : stmts ++ xs) strTable
getCreateVar (ForClassicStmt Nothing _ _ stmts : xs) strTable = getCreateVar (stmts ++ xs) strTable

getCreateVar (WhileStmt _ stmts : xs) strTable = getCreateVar (stmts ++ xs) strTable

getCreateVar (DoWhileStmt stmts _ : xs) strTable = getCreateVar (stmts ++ xs) strTable

getCreateVar (MatchStmt _ stmts : xs) strTable = getCreateVar (map snd stmts ++ xs) strTable

getCreateVar (_ : xs) strTable = getCreateVar xs strTable

module Compiler.Compile (
    -- compileStmt
    compile,
    getScopeFromList
) where

import Compiler.Type (Scope(..), OpCode(..), Compile(..))
import Parser.AST (Stmt(..), Expr(..))
import Compiler.CreateVar (getCreateVar, getFunctionVar)
import Compiler.Function (unsetFuncVar, getFunctionIndex)
import Compiler.CodingByte (getCodingByte)

-- Create a scope from a list of variable names
getScopeFromList :: [(OpCode, String)] -> String -> Int -> Scope
getScopeFromList [] _ _ = Scope [] "" 0
getScopeFromList var name start = Scope (map snd var) name start

-- add scope vars to another scope
addScope :: Scope -> Scope -> Scope
addScope (Scope vrs _ _) (Scope gVars name _) = Scope (vrs ++ gVars) name (length vrs)

-- get a list of SET_ARG for a function call
setFunctionArgs :: [Expr] -> Scope -> [String] -> [OpCode]
setFunctionArgs [] _ _ = []
setFunctionArgs exprs scope strTable = setFunctionArgs' exprs scope strTable 0
  where
    setFunctionArgs' :: [Expr] -> Scope -> [String] -> Int -> [OpCode]
    setFunctionArgs' [] _ _ _ = []
    setFunctionArgs' (x:xs) scop strTbl index =
        let codingByte = getCodingByte x scop strTbl
            opCode = OpSetArg index codingByte
        in opCode : setFunctionArgs' xs scop strTbl (index + 1)

compileCall :: Expr -> Scope -> Compile -> [OpCode]
compileCall (FuncCall (Var name) args) fScope comp = do
    let setArgs = setFunctionArgs args fScope (stringTable comp)
    let funcIndex = getFunctionIndex name comp
    setArgs ++ [OpCall funcIndex]
compileCall _ _ _ = []

compilePrint :: [Expr] -> Scope -> Compile -> String -> [OpCode]
compilePrint args scope comp print =
    let codingBytes = map (\arg -> getCodingByte arg scope (stringTable comp)) args
    in if print == "print" then map OpPrint codingBytes else map OpPrintLn codingBytes

-- get a list of opcode from an expr
compileExpr :: Expr -> Scope -> Compile -> [OpCode]
compileExpr (FuncCall (Var "print") args) scope comp = compilePrint args scope comp "print"
compileExpr (FuncCall (Var "printLn") args) scope comp = compilePrint args scope comp "printLn"
compileExpr (FuncCall name args) scope comp = compileCall (FuncCall name args) scope comp
compileExpr _ _ _ = []

-- get a list of opcode from an stmt
compileStmt :: Stmt -> Scope -> Compile -> [OpCode]
compileStmt (ExprStmt expr) scope comp = compileExpr expr scope comp
compileStmt (BlockStmt stmts) scope comp = concatMap (\stmt -> compileStmt stmt scope comp) stmts
compileStmt _ _ _ = []

-- get a list of opcode from a function
compileFunction :: Stmt -> Compile -> [OpCode]
compileFunction (FuncDeclStmt name args retType (Just body)) comp = do
    let createVars = getFunctionVar (FuncDeclStmt name args retType (Just body)) ++ getCreateVar [body] (stringTable comp)
    let fScope = addScope (globalScope comp) (getScopeFromList createVars name 0)
    let opcode = map fst createVars
    let unsetVars = unsetFuncVar (length args) (length $ vars $ globalScope comp)
    let bodyOpCode = compileStmt body fScope comp
    opcode ++ unsetVars ++ bodyOpCode ++ [OpReturn]
compileFunction _ _ = []

-- compileStmt :: [Stmt] 

compile :: [Stmt] -> Compile -> [[OpCode]]
compile [] _ = []
compile (FuncDeclStmt name args retType body : xs) compileData = compileFunction (FuncDeclStmt name args retType body) compileData : compile xs compileData
compile (_ : xs) compileData = compile xs compileData

module Compiler.Compile (
    -- compileStmt
    compile,
    getScopeFromList
) where

import Compiler.Type (Scope(..), OpCode(..), Compile(..))
import Parser.AST (Stmt(..), Expr(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..))
import Parser.Type (Type(..))
import Compiler.CreateVar (getCreateVar)

-- Create a scope from a list of variable names
getScopeFromList :: [(OpCode, String)] -> String -> Int -> Scope
getScopeFromList [] _ _ = Scope [] "" 0
getScopeFromList var name start = Scope (map snd var) name start

-- add scope vars to another scope
addScope :: Scope -> Scope -> Scope
addScope (Scope vars _ _) (Scope gVars name _) = Scope (vars ++ gVars) name (length vars)

-- get a list of opcode from a function
compileFunction :: Stmt -> Compile -> [OpCode]
compileFunction (FuncDeclStmt name args retType (Just body)) compile = do
    let createVars = getCreateVar [body] (stringTable compile)
    let fScope = addScope (globalScope compile) (getScopeFromList createVars name 0)
    let opcode = map fst createVars
    opcode

-- compileStmt :: [Stmt] 

compile :: [Stmt] -> Compile -> [[OpCode]]
compile [] _ = []
compile (FuncDeclStmt name args retType body : xs) compileData = compileFunction (FuncDeclStmt name args retType body) compileData : compile xs compileData
compile (_ : xs) compileData = compile xs compileData

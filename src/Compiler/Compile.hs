module Compiler.Compile (
    -- compileStmt
    compile,
    getScopeFromList,
    compileStmt,
    compileGlobal
) where

import Compiler.Type (Scope(..), OpCode(..), Compile(..), CodingByte(..), Function(..))
import Parser.AST (Stmt(..), Expr(..))
import Compiler.CreateVar (getCreateVar, getFunctionVar, getIndexFromStrTable)
import Compiler.Function (unsetFuncVar, getFunctionIndex)
import Compiler.CodingByte (getCodingByte)
import Compiler.Header (opListCountByte)
import Compiler.BinArith (compileBinArith)

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
compileCall (FuncCall (Var name) args) fScope comp =
    let setArgs = setFunctionArgs args fScope (stringTable comp)
        funcIndex = getFunctionIndex name comp
    in setArgs ++ [OpCall funcIndex]
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
compileExpr (BinArith op e1 e2) scope comp = compileBinArith (BinArith op e1 e2) scope comp (length $ vars scope) 0

compileExpr (Assign (Var x) (LitInt int)) scope _ = [OpSetVar (getIndexFromStrTable (vars scope) x) (CbConst 0xA0 0x01 int)]
compileExpr (Assign (Var x) (LitString str)) scope comp = [OpSetVar (getIndexFromStrTable (vars scope) x) (CbConst 0xA0 0x01 (getIndexFromStrTable (stringTable comp) str))]

compileExpr _ _ _ = []

-- get a list of opcode from an expr and set the result to the tmp register
compileExprToTmp :: Expr -> Scope -> Compile -> [OpCode]
compileExprToTmp (LitInt int) _ _ = [OpSetTmp 0x01 (CbConst 0xA0 0x01 int)]
compileExprToTmp (LitString str) _ comp = [OpSetTmp 0x02 (CbConst 0xA0 0x01 (getIndexFromStrTable (stringTable comp) str))]
compileExprToTmp (Var x) scope _ = [OpSetTmp 0x00 (CbConst 0xB0 0x00 (getIndexFromStrTable (vars scope) x ))]
compileExprToTmp _ _ _ = []

-- get a list of opcode from an stmt
compileStmt :: Stmt -> Scope -> Compile -> [OpCode]
compileStmt (LetStmt name _ expr) scope comp = compileExpr expr scope comp ++
    [OpSetVar (getIndexFromStrTable (vars scope) name) (CbConst 0xB0 0x00 (length $ vars scope))] ++
    [OpUnsetVar (length $ vars scope)]
compileStmt (ExprStmt expr) scope comp = compileExpr expr scope comp
compileStmt (BlockStmt stmts) scope comp = concatMap (\stmt -> compileStmt stmt scope comp) stmts
compileStmt (ReturnStmt (Just expr)) scope comp =
    let exprOpCode = compileExprToTmp expr scope comp
    in exprOpCode ++ [OpSetReturn 0x01 (CbConst 0xA0 0x01 0xffffffff)]
compileStmt _ _ _ = []

-- get a list of opcode from a function
compileFunction :: Stmt -> Compile -> [OpCode]
compileFunction (FuncDeclStmt name args retType (Just body)) comp =
    let createVars = getFunctionVar (FuncDeclStmt name args retType (Just body)) ++ getCreateVar [body] (stringTable comp)
        fScope = addScope (globalScope comp) (getScopeFromList createVars name 0)
        opcode = map fst createVars
        unsetVars = unsetFuncVar (length args) (length $ vars $ globalScope comp)
        bodyOpCode = compileStmt body fScope comp
    in opcode ++ unsetVars ++ bodyOpCode ++ [OpReturn]
compileFunction _ _ = []

-- return 0 if main returns an int, 1 otherwise
getMainReturnType :: [Function] -> Int
getMainReturnType [] = 1
getMainReturnType (x:xs) = if fName x == "main" then 0 else getMainReturnType xs

compileGlobal :: Stmt -> Compile -> Bool -> [OpCode]
compileGlobal stmts comp isMain =
    let createVars = getCreateVar [stmts] (stringTable comp)
        opcode = map fst createVars
        bodyOpCode = compileStmt stmts (globalScope comp) comp
        opCodeReturn = if getMainReturnType (functionTable comp) == 0
                   then [OpUnsetReturn 0xffffffff, OpSetReturn 0x01 (CbConst 0xB0 0x01 0xffffffff)]
                   else [OpSetReturn 0x01 (CbConst 0xA0 0x01 0x00)]
    in opcode ++ bodyOpCode ++ (if isMain then [OpCall 0] else []) ++ opCodeReturn ++ [OpReturn]

compile :: [Stmt] -> Compile -> [[OpCode]]
compile [] _ = []
compile (FuncDeclStmt name args retType body : xs) compileData = compileFunction (FuncDeclStmt name args retType body) compileData : compile xs compileData
compile (_ : xs) compileData = compile xs compileData

module Compiler.Compile (
    compile,
    getScopeFromList,
    compileStmt,
    compileGlobal,
    compileFunction
) where

import Compiler.Type (Scope(..), OpCode(..), Compile(..), CodingByte(..))
import Parser.Type (Type(..))
import Parser.AST (Stmt(..), Expr(..), CompOp(..))
import Compiler.CreateVar (getCreateVar, getFunctionVar, getIndexFromStrTable)
import Compiler.Function (unsetFuncVar, getFunctionIndex)
import Compiler.CodingByte (getCodingByte)
import Compiler.BinArith (compileBinArith, opCodeFromExpr)
import Compiler.Header (opListCountByte)
import Data.Either()

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

compileCall :: Expr -> Scope -> Compile -> Int -> [OpCode]
compileCall (FuncCall (Var name) args) fScope comp _ =
    let setArgs = setFunctionArgs args fScope (stringTable comp)
        funcIndex = getFunctionIndex name comp
    in setArgs ++ [OpCall funcIndex]
compileCall _ _ _ _ = []

compilePrint :: [Expr] -> Scope -> Compile -> String -> [OpCode]
compilePrint args scope comp prt =
    let codingBytes = map (\arg -> getCodingByte arg scope (stringTable comp)) args
    in if prt == "print" then map OpPrint codingBytes else map OpPrintLn codingBytes

-- get a list of opcode from an expr
compileExpr :: Expr -> Scope -> Compile -> Int -> [OpCode]
compileExpr (FuncCall (Var "print") args) scope comp _ = compilePrint args scope comp "print"
compileExpr (FuncCall (Var "printLn") args) scope comp _ = compilePrint args scope comp "printLn"
compileExpr (FuncCall name args) scope comp _ = compileCall (FuncCall name args) scope comp 0
compileExpr (BinArith op e1 e2) scope comp _ = compileBinArith (BinArith op e1 e2) scope comp (length $ vars scope) 0

compileExpr (Assign (Var x) (LitInt int)) scope _ _ = [OpSetVar (getIndexFromStrTable (vars scope) x) (CbConst 0xA0 0x01 int)]
compileExpr (Assign (Var x) (LitString str)) scope comp _ = [OpSetVar (getIndexFromStrTable (vars scope) x) (CbConst 0xA0 0x01 (getIndexFromStrTable (stringTable comp) str))]

compileExpr _ _ _ _ = []

-- get a list of opcode from an expr and set the result to the tmp register
compileExprToTmp :: Expr -> Scope -> Compile -> [OpCode]
compileExprToTmp (LitInt int) _ _ = [OpSetTmp 0x01 (CbConst 0xA0 0x01 int)]
compileExprToTmp (LitString str) _ comp = [OpSetTmp 0x02 (CbConst 0xA0 0x01 (getIndexFromStrTable (stringTable comp) str))]
compileExprToTmp (Var x) scope _ = [OpSetTmp 0x00 (CbConst 0xB0 0x00 (getIndexFromStrTable (vars scope) x ))]
compileExprToTmp (BinArith op e1 e2) scope comp =
    [OpCreateVar 0x01 0x00] ++
    compileExpr (BinArith op e1 e2) scope comp 0 ++
    [OpSetTmp 0x01 (CbConst 0xB0 0x01 (length $ vars scope))]
compileExprToTmp _ _ _ = []

isConst :: Expr -> Bool
isConst (LitInt _) = True
isConst (LitString _) = True
isConst (LitBool _) = True
isConst (Var _) = True
isConst _ = False

doBinComp :: CompOp -> Expr -> Expr -> Scope -> Compile -> [OpCode]
doBinComp op e1 e2 scope comp =
    case op of
        Equal -> [OpEq (opCodeFromExpr (Left e1) scope comp 0) (opCodeFromExpr (Left e2) scope comp 0)]
        NotEqual -> [OpNeq (opCodeFromExpr (Left e1) scope comp 0) (opCodeFromExpr (Left e2) scope comp 0)]
        LessThan -> [OpLesser (opCodeFromExpr (Left e1) scope comp 0) (opCodeFromExpr (Left e2) scope comp 0)]
        LessEqual -> [OpLesserEq (opCodeFromExpr (Left e1) scope comp 0) (opCodeFromExpr (Left e2) scope comp 0)]
        GreaterThan -> [OpGreater (opCodeFromExpr (Left e1) scope comp 0) (opCodeFromExpr (Left e2) scope comp 0)]
        GreaterEqual -> [OpGreaterEq (opCodeFromExpr (Left e1) scope comp 0) (opCodeFromExpr (Left e2) scope comp 0)]

compileBinComp :: Expr -> Scope -> Compile -> [OpCode]
compileBinComp (LitBool True) _ _ = [OpEq (CbConst 0xA0 0x01 0x01) (CbConst 0xA0 0x01 0x01)]
compileBinComp (LitBool False) _ _ = [OpEq (CbConst 0xA0 0x01 0x00) (CbConst 0xA0 0x01 0x01)]
compileBinComp (BinComp op e1 e2) scope comp = doBinComp op e1 e2 scope comp
compileBinComp _ _ _ = []

-- get a list of opcode from an stmt
compileStmt :: Stmt -> Scope -> Compile -> [OpCode]
compileStmt (LetStmt name _ expr) scope comp = if isConst expr == False
    then compileExpr expr scope comp 0 ++
        [OpCreateVar 0x01 0x00] ++
        [OpUnsetReturn (length $ vars scope)] ++
        [OpSetVar (getIndexFromStrTable (vars scope) name) (CbConst 0xB0 0x00 (length $ vars scope))] ++
        [OpUnsetVar (length $ vars scope)]
    else []

compileStmt (ExprStmt expr) scope comp = compileExpr expr scope comp 0

compileStmt (BlockStmt stmts) scope comp = concatMap (\stmt -> compileStmt stmt scope comp) stmts

compileStmt (ReturnStmt (Just expr)) scope comp =
    let exprOpCode = compileExprToTmp expr scope comp
    in exprOpCode ++ [OpSetReturn 0x01 (CbConst 0xB0 0x01 0xffffffff)] ++ [OpReturn]

compileStmt (IfStmt e1 s1 (Just s2)) scope comp =
    let trueStmt = compileStmt s1 scope comp
    in compileBinComp e1 scope comp ++
        [OpJump (opListCountByte trueStmt)] ++
        trueStmt ++
        compileStmt s2 scope comp
compileStmt (IfStmt e1 s1 Nothing) scope comp =
    let trueStmt = compileStmt s1 scope comp
    in compileBinComp e1 scope comp ++
        [OpJump (opListCountByte trueStmt)] ++
        trueStmt

compileStmt _ _ _ = []

-- get a list of opcode from a function
compileFunction :: Stmt -> Compile -> [OpCode]
compileFunction (FuncDeclStmt name args retType (Just body)) comp =
    let createVars = getFunctionVar (FuncDeclStmt name args retType (Just body)) ++ getCreateVar [body] (stringTable comp)
        fScope = addScope (globalScope comp) (getScopeFromList createVars name 0)
        opcode = map fst createVars
        unsetVars = unsetFuncVar (length args) ((length $ vars $ globalScope comp))
        bodyOpCode = compileStmt body fScope comp
    in opcode ++ unsetVars ++ bodyOpCode ++ if checkIfReturn [body] then [] else [OpReturn]
compileFunction _ _ = []

-- return 0 if main return type is int otherwise return 1
getMainReturnType :: [Stmt] -> Int
getMainReturnType [] = 1
getMainReturnType (FuncDeclStmt name _ retType _ : xs) = if name == "main" && retType == TInt then 0 else getMainReturnType xs
getMainReturnType (BlockStmt stmts:_) = getMainReturnType stmts
getMainReturnType (_:xs) = getMainReturnType xs

checkIfReturn :: [Stmt] -> Bool
checkIfReturn [] = False
checkIfReturn (ReturnStmt _:_) = True
checkIfReturn (_:xs) = checkIfReturn xs

compileGlobal :: Stmt -> Compile -> Bool -> [OpCode]
compileGlobal stmts comp isMain =
    let createVars = getCreateVar [stmts] (stringTable comp)
        opcode = map fst createVars
        bodyOpCode = compileStmt stmts (globalScope comp) comp
        opCodeReturn = if getMainReturnType [stmts] == 0
                   then [OpUnsetReturn 0xffffffff, OpSetReturn 0x01 (CbConst 0xB0 0x01 0xffffffff)]
                   else [OpSetReturn 0x01 (CbConst 0xA0 0x01 0x00)]
    in opcode ++ bodyOpCode ++ (if isMain then [OpCall 0] else []) ++ opCodeReturn ++ if checkIfReturn [stmts] then [] else [OpReturn]

compile :: [Stmt] -> Compile -> [[OpCode]]
compile [] _ = []
compile (FuncDeclStmt name args retType body : xs) compileData = compileFunction (FuncDeclStmt name args retType body) compileData : compile xs compileData
compile (_ : xs) compileData = compile xs compileData

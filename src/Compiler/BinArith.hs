module Compiler.BinArith (
    compileBinArith,
    opCodeFromExpr,
    compileCall,
    isReturnString,
    isBinArithString
) where

import Parser.AST
import Compiler.Type
import Parser.Type
import Data.Either()
import Compiler.CreateVar (getIndexFromStrTable)
import Compiler.Function (getFunctionIndex)
import Compiler.CodingByte (getCodingByte)

-- get a list of SET_ARG for a function call
setFunctionArgs :: [Expr] -> Scope -> [String] -> Compile -> Int -> [OpCode]
setFunctionArgs [] _ _ _ _ = []
setFunctionArgs exprs scope strTable comp depth = setFunctionArgs' exprs scope strTable 0 depth comp
  where
    setFunctionArgs' :: [Expr] -> Scope -> [String] -> Int -> Int -> Compile -> [OpCode]
    setFunctionArgs' [] _ _ _ _ _ = []
    setFunctionArgs' (x:xs) scop strTbl index depth' comp' =
        case x of
            BinArith op e1 e2 -> [OpCreateVar 0x01 0x00] ++
                compileBinArith (BinArith op e1 e2) scope comp' depth' 0 ++
                [OpSetArg index (CbConst 0xB0 0x01 depth')] ++
                [OpUnsetVar depth'] ++ setFunctionArgs' xs scop strTbl (index + 1) depth' comp'
            _ -> let codingByte = getCodingByte x scop strTbl
                     opCode = OpSetArg index codingByte
                 in opCode : setFunctionArgs' xs scop strTbl (index + 1) depth' comp'

compileCall :: Expr -> Scope -> Compile -> Int -> [OpCode]
compileCall (FuncCall (Var name) args) fScope comp depth =
    let setArgs = setFunctionArgs args fScope (stringTable comp) comp depth
        funcIndex = getFunctionIndex name comp
    in setArgs ++ [OpCall funcIndex]
compileCall _ _ _ _ = []

opCodeFromExpr :: Either Expr Int -> Scope -> Compile -> Int -> CodingByte
opCodeFromExpr (Left (LitInt int)) _ _ _ = CbConst 0xA0 0x01 int
opCodeFromExpr (Left (LitString str)) _ comp _ = CbConst 0xA0 0x01 (getIndexFromStrTable (stringTable comp) str)
opCodeFromExpr (Left (Var x)) scope _ _ = CbConst 0xB0 0x01 (getIndexFromStrTable (vars scope) x)
opCodeFromExpr (Right int) _ _ _ = CbConst 0xB0 0x01 int
opCodeFromExpr _ _ _ _ = CbConst 0x00 0x00 0x00

doBinArith :: ArithOp -> Either Expr Int -> Either Expr Int -> Scope -> Compile -> [OpCode]
doBinArith op e1 e2 scope comp =
    case op of
        Add -> [OpAdd (opCodeFromExpr e1 scope comp 0) (opCodeFromExpr e2 scope comp 1)]
        Subtract -> [OpSub (opCodeFromExpr e1 scope comp 0) (opCodeFromExpr e2 scope comp 1)]
        Multiply -> [OpMul (opCodeFromExpr e1 scope comp 0) (opCodeFromExpr e2 scope comp 1)]
        Divide -> [OpDiv (opCodeFromExpr e1 scope comp 0) (opCodeFromExpr e2 scope comp 1)]
        Modulo -> [OpMod (opCodeFromExpr e1 scope comp 0) (opCodeFromExpr e2 scope comp 1)]

isBinArithString :: Expr -> Bool
isBinArithString (LitString _) = True
isBinArithString (BinArith _ e1 e2) = isBinArithString e1 || isBinArithString e2
isBinArithString _ = False

isReturnString :: Stmt -> Bool
isReturnString (ReturnStmt (Just expr)) = isBinArithString expr
isReturnString (ReturnStmt Nothing) = False
isReturnString (LetStmt _ (Just t) _) = t == TString
isReturnString (LetStmt _ Nothing expr) = isBinArithString expr
isReturnString _ = False

compileBinArith :: Expr -> Scope -> Compile -> Int -> Int -> [OpCode]
compileBinArith (BinArith op (BinArith op1 ee1 ee2) (BinArith op2 ex1 ex2)) scope comp start depth =
        [OpCreateVar (if isBinArithString (BinArith op1 ee1 ee2) then 0x02 else 0x01) 0] ++  -- Create Var 1
            [OpCreateVar (if isBinArithString (BinArith op2 ex1 ex2) then 0x02 else 0x01) 0] ++ -- Create Var 2
            compileBinArith (BinArith op1 ee1 ee2) scope comp start (depth + 1) ++ -- Compile BinArith 1
            compileBinArith (BinArith op2 ex1 ex2) scope comp start (depth + 2) ++ -- Compile BinArith 2
            doBinArith op (Right (start + depth + 1)) (Right (start + depth + 2)) scope comp ++ -- Do BinArith with var 1 and var 2
            [OpUnsetReturn (start + depth)] ++ -- Unset the return value in the start index
            [OpUnsetVar (start + depth + 1)] ++ -- Unset the var 1
            [OpUnsetVar (start + depth + 2)] -- Unset the var 2

compileBinArith (BinArith op (FuncCall name args) (BinArith op1 e1 e2)) scope comp start depth =
    [OpCreateVar (if isBinArithString (FuncCall name args) then 0x02 else 0x01) 0] ++
        [OpCreateVar (if isBinArithString (BinArith op1 e1 e2) then 0x02 else 0x01) 0] ++
        compileCall (FuncCall name args) scope comp (start + depth + 3) ++
        [OpUnsetReturn (start + depth + 1)] ++
        compileBinArith (BinArith op1 e1 e2) scope comp start (depth + 2) ++
        doBinArith op (Right (start + depth + 1)) (Right (start + depth + 2)) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)] ++
        [OpUnsetVar (start + depth + 2)]

compileBinArith (BinArith op (BinArith op1 e1 e2) (FuncCall name args)) scope comp start depth =
    [OpCreateVar (if isBinArithString (BinArith op1 e1 e2) then 0x02 else 0x01) 0] ++
        [OpCreateVar (if isBinArithString (FuncCall name args) then 0x02 else 0x01) 0] ++
        compileBinArith (BinArith op1 e1 e2) scope comp start (depth + 1) ++
        compileCall (FuncCall name args) scope comp (start + depth + 3) ++
        [OpUnsetReturn (start + depth + 2)] ++
        doBinArith op (Right (start + depth + 1)) (Right (start + depth + 2)) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)] ++
        [OpUnsetVar (start + depth + 2)]

compileBinArith (BinArith op (FuncCall name1 args1) (FuncCall name2 args2)) scope comp start depth =
    [OpCreateVar (if isBinArithString (FuncCall name1 args1) then 0x02 else 0x01) 0] ++
        [OpCreateVar (if isBinArithString (FuncCall name2 args2) then 0x02 else 0x01) 0] ++
        compileCall (FuncCall name1 args1) scope comp (start + depth + 3) ++
        [OpUnsetReturn (start + depth + 1)] ++
        compileCall (FuncCall name2 args2) scope comp (start + depth + 3) ++
        [OpUnsetReturn (start + depth + 2)] ++
        doBinArith op (Right (start + depth + 1)) (Right (start + depth + 2)) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)] ++
        [OpUnsetVar (start + depth + 2)]

compileBinArith (BinArith op (FuncCall name args) e2) scope comp start depth =
    [OpCreateVar (if isBinArithString (FuncCall name args) then 0x02 else 0x01) 0] ++
        compileCall (FuncCall name args) scope comp (start + depth + 2) ++
        [OpUnsetReturn (start + depth + 1)] ++
        doBinArith op (Right (start + depth + 1)) (Left e2) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)]

compileBinArith (BinArith op e1 (FuncCall name args)) scope comp start depth =
    [OpCreateVar (if isBinArithString (FuncCall name args) then 0x02 else 0x01) 0] ++
        compileCall (FuncCall name args) scope comp (start + depth + 2) ++
        [OpUnsetReturn (start + depth + 1)] ++
        doBinArith op (Left e1) (Right (start + depth + 1)) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)]

compileBinArith (BinArith op (BinArith op1 ee1 ee2) e2) scope comp start depth =
    [OpCreateVar (if isBinArithString (BinArith op1 ee1 ee2) then 0x02 else 0x01) 0] ++
        compileBinArith (BinArith op1 ee1 ee2) scope comp start (depth + 1) ++
        doBinArith op (Right (start + depth + 1)) (Left e2) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)]

compileBinArith (BinArith op e1 (BinArith op1 ee1 ee2)) scope comp start depth =
    [OpCreateVar (if isBinArithString (BinArith op1 ee1 ee2) then 0x02 else 0x01) 0] ++
        compileBinArith (BinArith op1 ee1 ee2) scope comp start (depth + 1) ++
        doBinArith op (Left e1) (Right (start + depth + 1)) scope comp ++
        [OpUnsetReturn (start + depth)] ++
        [OpUnsetVar (start + depth + 1)]

compileBinArith (BinArith op e1 e2) scope comp start depth =
    doBinArith op (Left e1) (Left e2) scope comp ++ [OpUnsetReturn (start + depth)]

compileBinArith _ _ _ _ _ = []

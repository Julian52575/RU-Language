module Opcode (
    test
) where

import Data.Word (Word8, Word16)
import qualified Data.ByteString as B
import Data.Bits (shiftR, (.&.))
import Compiler.String (getStringTable)
import Compiler.Function(getFunctionTable, Function)
import Parser.AST (Stmt(..), Expr(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..))
import Parser.Type (Type(..))
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Compiler.CreateVar (getCreateVar)
import Compiler.Type (Scope(..), OpCode(..), Compile(..))
import Compiler.Compile (getScopeFromList, compile)

getIntFromExpr :: Expr -> Scope -> Int
getIntFromExpr (LitInt int) _ = int
getIntFromExpr (Var var) scope = if length (vars scope) == 0 then 0 else length (vars scope) - (fromJust $ elemIndex var (vars scope))
getIntFromExpr _ _ = 0

addToScope :: Scope -> String-> Scope
addToScope scope var = Scope (var : vars scope) (function scope) (indexStart scope)

listByteString :: [B.ByteString] -> B.ByteString
listByteString = B.concat

compileAdd :: Expr -> [String] -> Scope -> Int -> [OpCode]
compileAdd (BinArith Add e1 e2) strTable scope id = OpAdd (getIntFromExpr e1 scope) (getIntFromExpr e2 scope) : [OpUnsetReturn id]
compileAdd _ _ _ _ = []

compileBlock :: [Stmt] -> [String] -> Scope -> [OpCode]
compileBlock [] _ _ = []
compileBlock (LetStmt var _ (BinArith op e1 e2) : xs) strTable scope = compileAdd (BinArith op e1 e2) strTable scope (getIntFromExpr (Var var) (addToScope scope var)) ++ compileBlock xs strTable (addToScope scope var)
compileBlock (LetStmt var _ val : xs) strTable scope = OpCreateVar 0x01 (getIntFromExpr val scope) : compileBlock xs strTable (addToScope scope var)
compileBlock (ReturnStmt (Just val) : xs) strTable scope = OpSetReturn (getIntFromExpr val scope) : compileBlock xs strTable scope
compileBlock (_ : xs) strTable scope = compileBlock xs strTable scope

test :: [Stmt] -> IO ()
test ast = do
    let stringTable = nub $ getStringTable ast
    let functionTable = getFunctionTable ast stringTable
    let globalVars = getCreateVar ast stringTable
    let globalScope = getScopeFromList globalVars "global" 0
    let compileData = Compile stringTable functionTable globalScope

    print $ compileData
    print $ compile ast compileData


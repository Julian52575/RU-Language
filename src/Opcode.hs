module Opcode (
    test
) where

import Data.Word (Word8, Word16)
import qualified Data.ByteString as B
import Data.Bits (shiftR, (.&.))
import Compiler.String (getStringTable)
import Compiler.Function(getFunctionTable, Function)
import Parser.AST
import Parser.Type (Type(..))
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Compiler.CreateVar (getCreateVar)
import Compiler.Type (Scope(..), OpCode(..), Compile(..))
import Compiler.Compile (getScopeFromList, compile, compileStmt)
import Compiler.Header (getHeader, headerToByteString)

getIntFromExpr :: Expr -> Scope -> Int
getIntFromExpr (LitInt int) _ = int
getIntFromExpr (Var var) scope = if length (vars scope) == 0 then 0 else length (vars scope) - (fromJust $ elemIndex var (vars scope))
getIntFromExpr _ _ = 0

addToScope :: Scope -> String-> Scope
addToScope scope var = Scope (var : vars scope) (function scope) (indexStart scope)

test :: [Stmt] -> IO ()
test ast = do
    let stringTable = nub $ getStringTable ast
    let functionTable = getFunctionTable ast stringTable
    let globalVars = getCreateVar ast stringTable
    let globalScope = getScopeFromList globalVars "global" 0
    let compileData = Compile stringTable functionTable globalScope
    let compiled = compile ast compileData
    let header = getHeader compileData compiled
    let headerByteString = headerToByteString header

    -- print $ compileData
    print $ compiled
    print $ header
    B.writeFile "out.bin" headerByteString

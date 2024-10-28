module Opcode (
    test
) where

import qualified Data.ByteString as B
import Compiler.String (getStringTable)
import Compiler.Function(getFunctionTable, getFunctionIndex)
import Parser.AST
import Data.List (nub)
import Compiler.CreateVar (getCreateVar)
import Compiler.Type (OpCode(..), Compile(..), Function(..))
import Compiler.Compile (getScopeFromList, compile, compileGlobal)
import Compiler.Header (getHeader, headerToByteString, opCodeToByteString)

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt _ _ [] = []
swapElementsAt a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list

swapMainFunction :: Compile -> [[OpCode]] -> (Compile, [[OpCode]])
swapMainFunction comp [] = (comp, [])
swapMainFunction comp (x:[]) = (comp, [x])
swapMainFunction comp opcodes = do
    let mainIndex = (getFunctionIndex "main" comp)
    let swapedOpCode = swapElementsAt 0 mainIndex opcodes
    let functionList = swapElementsAt 0 mainIndex (functionTable comp)
    ((Compile (stringTable comp) (functionList) (globalScope comp)), swapedOpCode)

isMain :: [Function] -> Bool
isMain [] = False
isMain (x:xs) = if fName x == "main" then True else isMain xs

test :: [Stmt] -> IO ()
test ast = do
    let stringTbl = nub $ getStringTable ast
    let functionTbl = getFunctionTable ast stringTbl
    let globalVars = getCreateVar ast stringTbl
    let sGlobal = getScopeFromList globalVars "global" 0
    let compileData = Compile stringTbl functionTbl sGlobal
    let compiled = compile ast compileData
    let globalCompiled = compileGlobal (BlockStmt ast) compileData (isMain functionTbl)
    let swapData = swapMainFunction compileData compiled
    let compileData' = fst swapData
    let compiled' = snd swapData
    let header = getHeader compileData' globalCompiled compiled'
    let headerByteString = headerToByteString header
    let codeByteString = B.pack $ opCodeToByteString $ globalCompiled ++ (concat compiled')

    B.writeFile "out.bin" $ B.append headerByteString codeByteString

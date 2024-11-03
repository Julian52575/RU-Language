module Opcode (
    compileAst
) where

import qualified Data.ByteString as B
import Compiler.String (getStringTable)
import Compiler.Function(getFunctionTable, getFunctionIndex)
import Parser.AST
import Data.List (nub, partition)
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
swapMainFunction comp opcodes =
    let mainIndex = (getFunctionIndex "main" comp)
        swapedOpCode = swapElementsAt 0 mainIndex opcodes
        functionList = swapElementsAt 0 mainIndex (functionTable comp)
    in case mainIndex of
        0 -> (comp, opcodes)
        _ -> ((Compile (stringTable comp) (functionList) (globalScope comp)), swapedOpCode)

isMain :: [Function] -> Bool
isMain [] = False
isMain (x:xs) = if fName x == "main" then True else isMain xs

containMain :: [Stmt] -> Bool
containMain [] = False
containMain (x:xs) = case x of
    FuncDeclStmt "main" _ _ _ -> True
    _ -> containMain xs

moveMainToFront :: [Function] -> [Function]
moveMainToFront functions = case partition (\f -> fName f == "main") functions of
    ([], rest) -> rest
    (mainFunc, rest) -> mainFunc ++ rest

compileAst :: [Stmt] -> String -> IO ()
compileAst ast filename =
    let stringTbl = [""] ++ (nub $ getStringTable ast)
        functionTbl = getFunctionTable ast stringTbl
        globalVars = getCreateVar ast stringTbl
        sGlobal = getScopeFromList globalVars "global" 0
        compileData = Compile stringTbl functionTbl sGlobal
        compiled = compile ast (compileData { functionTable = (moveMainToFront functionTbl)})
        globalCompiled = compileGlobal (BlockStmt ast) compileData (isMain functionTbl)
        swapData = if containMain ast then swapMainFunction compileData compiled else (compileData, compiled)
        compileData' = fst swapData
        compiled' = snd swapData
        header = getHeader compileData' globalCompiled compiled'
        headerByteString = headerToByteString header
        codeByteString = B.pack $ opCodeToByteString $ globalCompiled ++ (concat compiled')

    in B.writeFile filename $ B.append headerByteString codeByteString

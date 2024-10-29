module RuInstructionsModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule
import RuVariableModule
import RuFormatModule

data RuInstruction = RuInstruction {
    ruInstructionPrefix :: Word8,
    ruInstructionInfix :: Word8,
    ruInstructionName :: String,
    ruInstructionFunction :: (RuVmInfo -> RuVmState -> Either RuException RuVmState),
    fixedSize :: Word32 --Taille de l'instruction si pas coding byte
}

{-- NOOP
 --}
ruInstructionNoop :: RuInstruction
ruInstructionNoop = RuInstruction {
    ruInstructionPrefix = 0x00,
    ruInstructionInfix = 0x00,
    ruInstructionName = "NOOP",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 2
}
ruInstructionFunctionNoop :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionNoop _ state = Right state

{-- print
 --}
ruInstructionPrint :: RuInstruction
ruInstructionPrint = RuInstruction {
    ruInstructionPrefix = 0x00,
    ruInstructionInfix = 0x01,
    ruInstructionName = "PRINT",
    ruInstructionFunction = ruInstructionFunctionPrint,
    fixedSize = 2
}

ruInstructionFunctionPrint :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionPrint _ state = do
    let ccode = workerCode state
    --let codeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 3 then Left ruExceptionIncompleteInstruction
    else do
        let operand = ccode !! 2
        let newState = state { toPrint = show operand }
        Right newState

{-- PrintLn
 --}
ruInstructionPrintLn :: RuInstruction
ruInstructionPrintLn = RuInstruction {
    ruInstructionPrefix = 0x00,
    ruInstructionInfix = 0x02,
    ruInstructionName = "PRINTLN",
    ruInstructionFunction = ruInstructionFunctionPrintLn,
    fixedSize = 2
}

ruInstructionFunctionPrintLn :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionPrintLn _ state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 3 then Left ruExceptionIncompleteInstruction
    else do
        let operand = ccode !! 2
        let newState = state { toPrint = show operand ++ "\n" }
        Right newState

{-- Create var
--}
ruInstructionCreateVar :: RuInstruction
ruInstructionCreateVar = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x00,
    ruInstructionName = "CREATEVAR",
    ruInstructionFunction = ruInstructionFunctionCreateVar,
    fixedSize = 10
}

getWord32FromOperand :: [Word8] -> Word32
getWord32FromOperand operand = do
    let var = word8ArrayToWord32 operand
    if var == Nothing then 0
    else fromJust var

ruInstructionGetVariableFromCode :: [Word8] -> RuVmInfo -> RuVariable
ruInstructionGetVariableFromCode ccode info = do
    let operand1 = take 4 (drop 2 ccode)
    let operand2 = take 4 (drop 6 ccode)
    let var = defaultRuVariable { ruVariableType = (operand1 !! 3) }
    if operand1 !! 3 == ruVariableTypeInt then var { ruVariableValue = Int32 (getWord32FromOperand operand2) }
    else if operand1 !! 3 == ruVariableTypeStr then var { ruVariableValue = Str (case ruVmInfoGetStringFromStringTable info (getWord32FromOperand operand2) of
        Nothing -> ""
        Just str -> str) }
    else var { ruVariableValue = Na }

ruInstructionFunctionCreateVar :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionCreateVar vminfo state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 10 then Left ruExceptionIncompleteInstruction
    else do
        let var = ruInstructionGetVariableFromCode ccode vminfo
        let newVariables = ruVmVariablesSetVariableInCurrentScope (variables state) var
        let newState = state { variables = newVariables }
        Right newState

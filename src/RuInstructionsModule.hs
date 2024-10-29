module RuInstructionsModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule
import RuVariableModule
import RuFormatModule
import RuOperandModule

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

ruInstructionGetRuVariableFromBytes :: [Word8] -> [Word8] -> RuOperand -> RuVmInfo -> RuVmState -> Maybe RuVariable
ruInstructionGetRuVariableFromBytes operand1 operand2 codingOperand info state
    | operand1 !! 3 == ruVariableTypeInt && codingOperand == RuOperandConstant = Just defaultRuVariable { ruVariableType = 0x01, ruVariableValue = Int32 (getWord32FromOperand operand2) }
    | operand1 !! 3 == ruVariableTypeStr && codingOperand == RuOperandConstant = Just defaultRuVariable { ruVariableType = 0x02, ruVariableValue = Str (case ruVmInfoGetStringFromStringTable info (getWord32FromOperand operand2) of
        Nothing -> ""
        Just str -> str) }
    -- | RuOperand !! 1 == 0b11 = var
    | otherwise = var
        where
            var = ruVmVariablesGetVariableInCurrentScope (variables state) (getWord32FromOperand operand2)

ruInstructionSetTmpVar :: RuInstruction
ruInstructionSetTmpVar = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x02,
    ruInstructionName = "SETTMPVAR",
    ruInstructionFunction = ruInstructionFunctionSetTmpVar,
    fixedSize = 0
}

ruInstructionFunctionSetTmpVar :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionSetTmpVar vminfo state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 11 then Left ruExceptionIncompleteInstruction
    else do
        let codingByte = take 1 (drop 2 ccode)
        let codingOperand = codingByteToRuOperand (codingByte !! 0)
        let operand1 = take 4 (drop 3 ccode)
        let operand2 = take 4 (drop 7 ccode)
        let var = ruInstructionGetRuVariableFromBytes operand1 operand2 (codingOperand !! 1) vminfo state
        if var == Nothing then Left $ ruExceptionIncompleteInstruction
        else do
            let newVariables = (variables state) { tmpVariable = fromJust var }
            let newState = state { variables = newVariables }
            Right newState
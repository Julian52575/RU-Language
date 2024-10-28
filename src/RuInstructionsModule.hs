module RuInstructionsModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule

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


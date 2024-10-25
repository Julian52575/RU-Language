module RuInstructionsModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule

data RuInstruction = RuInstruction {
    ruInstructionPrefix :: Word8,
    ruInstructionInfix :: Word8,
    ruInstructionName :: String,
    ruInstructionFunction :: (RuVmState -> Either RuException RuVmState),
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
ruInstructionFunctionNoop :: RuVmState -> Either RuException RuVmState
ruInstructionFunctionNoop state = Right state

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

ruInstructionFunctionPrint :: RuVmState -> Either RuException RuVmState
ruInstructionFunctionPrint state = do
    let code = workerCode state
    let codeOffset = workerCodeOffset state
    let codeSize = length code
    if codeSize < 3 then Left ruExceptionIncompleteInstruction
    else do
        let operand = code !! 2
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

ruInstructionFunctionPrintLn :: RuVmState -> Either RuException RuVmState
ruInstructionFunctionPrintLn state = do
    let code = workerCode state
    let codeOffset = workerCodeOffset state
    let codeSize = length code
    if codeSize < 3 then Left ruExceptionIncompleteInstruction
    else do
        let operand = code !! 2
        let newState = state { toPrint = show operand ++ "\n" }
        Right newState

{-- List
 --}
ruInstructionListPrefix0x00 :: [ RuInstruction ]
ruInstructionListPrefix0x00 = [ ruInstructionNoop ]

ruInstructionList :: [ [RuInstruction] ]
ruInstructionList = [ ruInstructionListPrefix0x00 ]


{-- Helper function
 --}

codingByteToOperandsSize :: Word8 -> Word32
codingByteToOperandsSize i = 0xff --TODO

{--
 --}

getRuInstruction :: Word8 -> Word8 -> Maybe RuInstruction
getRuInstruction insPrefix insInfix
    | prefixInt >= length ruInstructionList = Nothing
    | infixInt >= length prefixList = Nothing
    | otherwise = Just instruction
    where
    prefixInt = fromIntegral insPrefix
    infixInt = fromIntegral insInfix
    prefixList = ruInstructionList !! prefixInt
    instruction = prefixList !! infixInt

getInstructionFunction :: Word8 -> Word8 -> Maybe (RuVmState -> Either RuException RuVmState)
getInstructionFunction insPrefix insInfix =
    case getRuInstruction insPrefix insInfix of
        Nothing -> Nothing
        Just ins -> Just (ruInstructionFunction ins)

getInstructionFixedSize :: Word8 -> Word8 -> Maybe (Word32)
getInstructionFixedSize insPrefix insInfix =
    case getRuInstruction insPrefix insInfix of
        Nothing -> Nothing
        Just ins -> Just (fixedSize ins)


module RuInstructionsHelperModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule
import RuInstructionsModule

{-- List
 --}
-- Builtin
ruInstructionListPrefix0x00 :: [ RuInstruction ]
ruInstructionListPrefix0x00 = [ ruInstructionNoop, ruInstructionPrint, ruInstructionPrintLn ]

--Variable management
ruInstructionListPrefix0x01 :: [ RuInstruction ]
ruInstructionListPrefix0x01 = [ ruInstructionCreateVar, ruInstructionSetVar, ruInstructionSetTmpVar, ruInstructionUnsetArg, ruInstructionSetReturn, ruInstructionUnsetReturn, ruInstructionDeleteVar ]

--Control flow
ruInstructionListPrefix0x02 :: [ RuInstruction ]
ruInstructionListPrefix0x02 = [ ]

--Arithmetic
ruInstructionListPrefix0x03 :: [ RuInstruction ]
ruInstructionListPrefix0x03 = [ ruInstructionAdd, ruInstructionSub, ruInstructionDiv, ruInstructionMul, ruInstructionEq, ruInstructionNeq, ruInstructionMod, ruInstructionLesser, ruInstructionLesserEq, ruInstructionGreater, ruInstructionGreaterEq ]

ruInstructionList :: [ [RuInstruction] ]
ruInstructionList = [ ruInstructionListPrefix0x00, ruInstructionListPrefix0x01, ruInstructionListPrefix0x02, ruInstructionListPrefix0x03 ]

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

getInstructionFunction :: Word8 -> Word8 -> Maybe (RuVmInfo -> RuVmState -> Either RuException RuVmState)
getInstructionFunction insPrefix insInfix =
    case getRuInstruction insPrefix insInfix of
        Nothing -> Nothing
        Just ins -> Just (ruInstructionFunction ins)

getInstructionFixedSize :: Word8 -> Word8 -> Maybe (Word32)
getInstructionFixedSize insPrefix insInfix =
    case getRuInstruction insPrefix insInfix of
        Nothing -> Nothing
        Just ins -> Just (fixedSize ins)

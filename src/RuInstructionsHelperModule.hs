module RuInstructionsHelperModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule
import RuInstructionsModule

{-- List
 --}
ruInstructionListPrefix0x00 :: [ RuInstruction ]
ruInstructionListPrefix0x00 = [ ruInstructionNoop, ruInstructionPrint, ruInstructionPrintLn ]

ruInstructionList :: [ [RuInstruction] ]
ruInstructionList = [ ruInstructionListPrefix0x00 ]

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

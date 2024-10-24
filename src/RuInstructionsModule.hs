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


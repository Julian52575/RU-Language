module RuOperandModule where

import Data.Maybe
import Data.Word
import Data.Bits

data RuOperand =
    RuOperandNone       |
    RuOperandUnused     |
    RuOperandConstant   |
    RuOperandVariableId
    deriving (Eq, Show)

{-- Helper function
 --}
ruOperandToWord8 :: RuOperand -> Word8
ruOperandToWord8 RuOperandVariableId = 0x03 --0b11
ruOperandToWord8 RuOperandConstant   = 0x02 --0b10
ruOperandToWord8 RuOperandUnused     = 0x01 --0b01
ruOperandToWord8 _                   = 0x00 --0b00

word8ToRuOperand :: Word8 -> RuOperand
word8ToRuOperand duoBito
    | duoBito == (ruOperandToWord8 RuOperandVariableId) = RuOperandVariableId
    | duoBito == (ruOperandToWord8 RuOperandConstant)   = RuOperandConstant
    | duoBito == (ruOperandToWord8 RuOperandUnused)     = RuOperandUnused
    | otherwise                                         = RuOperandNone

ruOperandToSize :: RuOperand -> Word32
ruOperandToSize RuOperandVariableId = 0x04 --0b11
ruOperandToSize RuOperandConstant   = 0x04 --0b10
ruOperandToSize RuOperandUnused     = 0x00 --0b01
ruOperandToSize _                   = 0x00 --0b00

codingByteToRuOperand :: Word8 -> [RuOperand]
codingByteToRuOperand cb  = [result1] ++ [result2] ++ [result3] ++ [result4]
    where
    op1Int = (cb .&. 0xC0) `shiftR` 6  --11 00 00 00
    op2Int = (cb .&. 0x30) `shiftR` 4  --00 11 00 00
    op3Int = (cb .&. 0x0C) `shiftR` 2  --00 00 11 00
    op4Int = (cb .&. 0x03)             --00 00 00 11
    result1 = word8ToRuOperand op1Int
    result2 = word8ToRuOperand op2Int
    result3 = word8ToRuOperand op3Int
    result4 = word8ToRuOperand op4Int
    
parseCodingByteListToSize :: [RuOperand] -> Word32 -> Word32
parseCodingByteListToSize (x:xs) size = parseCodingByteListToSize xs newSize
    where
        newSize = size + ruOperandToSize x
parseCodingByteListToSize [] size = size

codingByteToOperandsSize :: Word8 -> Word32
codingByteToOperandsSize cb = parseCodingByteListToSize list 0x00
        where
            list = codingByteToRuOperand cb


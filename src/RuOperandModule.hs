module RuOperandModule where

import Data.Word

data RuOperand =
    RuOperandNone       |
    RuOperandUnused     |
    RuOperandConstant   |
    RuOperandVariableId
    deriving (Eq, Show)


ruOperandToWord8 :: ruOperand -> Word8
ruOperandToWord8 ruOperandNone       = 0x00 --0b00
ruOperandToWord8 ruOperandUnused     = 0x01 --0b01
ruOperandToWord8 ruOperandConstant   = 0x02 --0b10
ruOperandToWord8 ruOperandVariableId = 0x03 --0b11

codingByteToOperandsSize :: Word8 -> Word32
codingByteToOperandsSize i = 0xff --TODO



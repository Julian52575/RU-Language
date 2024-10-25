module RuOperandModule where

import Data.Word

data RuOperand =
    RuOperandNone       |
    RuOperandUnused     |
    RuOperandConstant   |
    RuOperandVariableId
    deriving (Eq, Show)

{--
ruOperandToWord8 :: ruOperand -> Word8
ruOperandToWord8 RuOperandNone       = 0x00 --0b00
ruOperandToWord8 RuOperandUnused     = 0x01 --0b01
ruOperandToWord8 RuOperandConstant   = 0x02 --0b10
ruOperandToWord8 RuOperandVariableId = 0x03 --0b11
--}
codingByteToOperandsSize :: Word8 -> Word32
codingByteToOperandsSize i = 0xff --TODO



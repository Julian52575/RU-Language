module RuInstructionsModule where

import Data.Word (Word8)
import RuVmModule

data RuInstruction = RuInstruction {
    ruInstructionMnemonic :: Word8,
    ruInstructionName :: String,
    ruInstructionFunction :: (RuVm -> [Word8] -> [Word8])
}

ruInstructionListPrefix0x00 :: [ RuInstruction ]
ruInstructionListPrefix0x00 = []

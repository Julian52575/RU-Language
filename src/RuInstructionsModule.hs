module RuInstructionsModule where

import Data.Word (Word8, Word32)

import RuVmModule

data RuInstruction = RuInstruction {
    ruInstructionMnemonic :: Word8,
    ruInstructionName :: String,
    ruInstructionFunction :: (RuVmInfo -> RuVmState -> RuVmState),
    fixedSize :: Word32 --Taille de l'instruction si pas coding byte
}

ruInstructionListPrefix0x00 :: [ RuInstruction ]
ruInstructionListPrefix0x00 = []

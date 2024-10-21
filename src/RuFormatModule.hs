module RuFormatModule where

import Data.Word (Word64)

data RuFunctionTable = RuFunctionTable {
    nameIndex :: Word64,
    codeOffset :: Word64,
    size :: Word64
} deriving (Eq, Show)

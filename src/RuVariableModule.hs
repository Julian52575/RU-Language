module RuVariableModule where

import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word64)

data RuVariableValue =
    Na |
    Int64 Word64 |
    Str String
    deriving (Eq, Show)

data RuVariable = RuVariable {
    ruVariableValue :: RuVariableValue,
    ruMutable :: Bool
} deriving (Eq, Show)


ruVariableGetStr :: RuVariable -> Maybe String
ruVariableGetStr (RuVariable { ruVariableValue = Str s }) = Just s
ruVariableGetStr _ = Nothing

ruVariableGetInt :: RuVariable -> Maybe Word64
ruVariableGetInt (RuVariable { ruVariableValue = Int64 i }) = Just i
ruVariableGetInt _ = Nothing



module RuVariableModule where

import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32)

data RuVariableValue =
    Na |
    Int32 Word32 |
    Str String
    deriving (Eq, Show)

ruVariableValueGetVariableType :: RuVariableValue -> Word8
ruVariableValueGetVariableType (Int32 _) = ruVariableTypeInt
ruVariableValueGetVariableType (Str _) = ruVariableTypeStr
ruVariableValueGetVariableType _ = 0x00

data RuVariable = RuVariable {
    ruVariableValue :: RuVariableValue,
    ruVariableType :: Word8,
    ruVariableId :: Word32,
    ruMutable :: Bool
} deriving (Eq, Show)

defaultRuVariable :: RuVariable
defaultRuVariable = RuVariable {
    ruVariableValue = Na,
    ruVariableType = 0x00,
    ruVariableId = 0x00,
    ruMutable = True
}

getRuVariableValueAsString :: RuVariableValue -> String
getRuVariableValueAsString (Int32 i) = show i
getRuVariableValueAsString (Str s) = s
getRuVariableValueAsString Na = "No type attributed."

printRuVariable :: RuVariable -> IO ()
printRuVariable var = putStrLn (getRuVariableValueAsString (ruVariableValue var) )
printRuVariable _ = putStrLn "Empty RuVariable"

{-- Helper function for defining variable type
 --}
ruVariableTypeInt :: Word8
ruVariableTypeInt = 0x01

ruVariableTypeStr :: Word8
ruVariableTypeStr = 0x02

ruVariableTypeFunction :: Word8
ruVariableTypeFunction = 0x03

ruVariableTypeVariableId :: Word8
ruVariableTypeVariableId = 0x04

{-- Helper function for getting / checking variable type
 --}
ruVariableGetType :: RuVariable -> Word8
ruVariableGetType (RuVariable { ruVariableType = i }) = i

ruVariableIsType :: RuVariable -> Word8 -> Bool
ruVariableIsType (RuVariable { ruVariableType = i }) t = i == t

ruVariableHasId :: RuVariable -> Word32 -> Bool
ruVariableHasId var idd = (ruVariableId var == idd)

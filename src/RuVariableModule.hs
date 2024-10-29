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

printRuVariable :: RuVariable -> IO ()
printRuVariable variable
    | typ == ruVariableTypeInt = putStrLn (show (ruVariableValue variable)) 
    | typ == ruVariableTypeStr = putStrLn (show (ruVariableValue variable)) 
    where
        typ = ruVariableType variable
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

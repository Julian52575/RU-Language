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
    ruVariableType :: Int,
    ruMutable :: Bool
} deriving (Eq, Show)

defaultRuVariable :: RuVariable
defaultRuVariable = RuVariable {
    ruVariableValue = Na,
    ruVariableType = 0x00,
    ruMutable = True
}

printRuVariable :: RuVariable -> IO ()
printRuVariable variable
    | typ == ruVariableTypeInt = putStrLn (show (ruVariableValue variable)) 
    | typ == ruVariableTypeStr = putStrLn (show (ruVariableValue variable)) 
    where
        typ = ruVariableType variable

{-- Helper function for defining variable type
 --}
ruVariableTypeInt :: Int
ruVariableTypeInt = 0x01

ruVariableTypeStr :: Int
ruVariableTypeStr = 0x02

ruVariableTypeFunction :: Int
ruVariableTypeFunction = 0x03

ruVariableTypeVariableId :: Int
ruVariableTypeVariableId = 0x04

{-- Helper function for getting / checking variable type
 --}
ruVariableGetType :: RuVariable -> Int
ruVariableGetType (RuVariable { ruVariableType = i }) = i

ruVariableIsType :: RuVariable -> Int -> Bool
ruVariableIsType (RuVariable { ruVariableType = i }) t = i == t

{-- Helper function to create a RuVariable
 --}
makeRuVariable :: RuVariableValue -> Int -> Bool -> RuVariable
makeRuVariable value typ mutable = RuVariable {
    ruVariableValue = value,
    ruVariableType = typ,
    ruMutable = mutable
}

module RuVmModule where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Either

import RuVariableModule
import RuFormatModule as RF
import RuExceptionModule


data RuVmState = RuVmState {
    variableStack :: [RuVariable],
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8]
} deriving (Eq, Show)

data RuVm = RuVm {
    stringTable :: [String], --the first string must be '\0'
    functionTable :: [RuFunctionTable],
    code :: [Word8],
    codeSize :: Word32,
    ruVmState :: RuVmState
} deriving (Eq, Show)


{-- Get a RuVm from a fileName
 --}
fileNameToRuVm :: String -> IO RuVm
fileNameToRuVm fileName = do
    byteString <- BS.readFile fileName
    let byteList = BS.unpack byteString
    return RuVm {
        stringTable = ["Hello", "World"],
        functionTable = [],
        code = byteList
    }

{-- Helper function to update program counter
 --}
ruVmUpdateWorkerCodeOffset :: RuVm -> Int -> Either RuException RuVm
ruVmUpdateWorkerCodeOffset vm offset = Right vm

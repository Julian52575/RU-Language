module RuVmModule where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Either

import RuVariableModule
import RuFormatModule
import RuExceptionModule


data RuVmState = RuVmState {
    variableStack :: [RuVariable],
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8]
} deriving (Eq, Show)

data RuVm = RuVm {
    fileVersion :: Word8,
    stringTable :: [String], --the first string must be '\0'
    functionTable :: [RuFunctionTable],
    code :: [Word8],
    codeSize :: Word32,
    ruVmState :: RuVmState
} deriving (Eq, Show)


{--
readFileAsWord8 :: String -> [Word8]
readFileAsWord8 fileName = do
    byteString <- DB.readFile fileName
    return byteString --}

-- Function to read a file and return its content as a [Word8]
readFileAsWord8 :: FilePath -> IO [Word8]
readFileAsWord8 filePath = do
    -- Read the file as ByteString
    byteString <- BS.readFile filePath
    -- Convert ByteString to a list of Word8
    let byteList = BS.unpack byteString
    return byteList

removeIo :: [Word8] -> [Word8]
removeIo list = list

{-- Get a RuVm from a fileName
 --}
fileNameToRuVm :: String -> IO RuVm
fileNameToRuVm fileName = do
    ioByteList <- readFileAsWord8 fileName
    let byteList = removeIo(ioByteList)
    return RuVm {
        fileVersion = 1,
        stringTable = ["Hello", "World"],
        functionTable = [],
        code = byteList
    }

{-- Helper function to update program counter
 --}
ruVmUpdateWorkerCodeOffset :: RuVm -> Int -> Either RuException RuVm
ruVmUpdateWorkerCodeOffset vm offset = Right vm

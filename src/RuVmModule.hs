module RuVmModule where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Char
import Data.Either
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import RuVariableModule
import RuFormatModule as RF
import RuExceptionModule


data RuVmState = RuVmState {
    variableStack :: [RuVariable],
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8],
    conditionalMode :: Bool
} deriving (Eq, Show)

data RuVmInfo = RuVmInfo {
    stringTable :: [String], --the first string must be '\0'
    functionTable :: [RuFunctionTable],
    code :: [Word8],
    codeSize :: Word32
    --vmState :: RuVmState -- Separate to ensure only VmState is updated ?
} deriving (Eq, Show)


convertWord8ToStringTable :: [Word8] -> String -> [String]
convertWord8ToStringTable (w:ws) currentStr
    | w == 0x00 = [ (currentStr ++ "\0") ] ++ convertWord8ToStringTable ws []
    | 0x01 <= w && w <= 0x7f = convertWord8ToStringTable ws (currentStr ++ [wCharTrue])
    | otherwise = convertWord8ToStringTable ws (currentStr ++ [wChar])
    where
        wInt = fromIntegral w
        wChar = '.'
        wCharTrue = chr (fromIntegral w)
convertWord8ToStringTable (w:_) currentStr 
    | w == 0x00 = [ currentStr ++ "\0" ]
    | 0x01 <= w && w <= 0x7f = [ currentStr ++ [wCharTrue] ++ "\0" ]
    | otherwise = [ currentStr ++ [wChar] ++ "\0" ]
    where
        wInt = fromIntegral w
        wChar = '.'
        wCharTrue = chr (fromIntegral w)
convertWord8ToStringTable [] _ = []


convertWord8ToFunctionTable :: [Word8] -> [RuFunctionTable]
convertWord8ToFunctionTable (n:a:m:e:o:f:f2:t:s:i:z:e2:next) = do
    [ fun ] ++ convertWord8ToFunctionTable next
    where
    fun = RuFunctionTable {
        nameIndex = word84ToWord32 n a m e,
        codeSectionOffset = word84ToWord32 o f f2 t,
        size = word84ToWord32 s i z e2
    }
convertWord8ToFunctionTable [] = []

ruFormatToRuVmInfo :: RuFormat -> Either RuException RuVmInfo
ruFormatToRuVmInfo format = do
    let intFileSize = fromIntegral (fileSize (ruHeader format))
    let intCodeOffset = fromIntegral (codeOffset (ruHeader format))
    if intCodeOffset >= intFileSize - 64
        then Left ruExceptionInvalidCodeOffset
        else do
            let size = intFileSize - intCodeOffset
            Right RuVmInfo {
                stringTable = convertWord8ToStringTable (strTab format) "\0",
                functionTable = convertWord8ToFunctionTable (ruFunctionTable format),
                code = codeSection format,
                codeSize =  fileSize (ruHeader format)
            }
            where 


{-- Get a RuVm from a fileName
 --}
fileNameToRuVm :: String -> IO (Either RuException RuVmInfo)
fileNameToRuVm fileName = runExceptT $ do --lire fichier
    byteString <- liftIO $ BS.readFile fileName
    let byteList = BS.unpack byteString
    let result = RF.fileContentToRuFormat byteList
    if isLeft result
        then throwE (fromLeft ruExceptionGenericFileError result)
        else do --get format from right
        let format = fromRight defaultRuFormat result
        if format == defaultRuFormat
            then throwE ruExceptionGenericFileError
            else do --convert format to VmInfo
                let result2 = (ruFormatToRuVmInfo format)
                if isLeft result2
                    then throwE (fromLeft ruExceptionGenericFileError result2)
                    else do --return vmInfo. Fuck fromRight default value
                        let vmInfo = fromRight ( RuVmInfo {
                            stringTable = [],
                            functionTable = [],
                            code = [],
                            codeSize = 0
                        }) result2
                        return vmInfo

{-- Helper function to update program counter
 --}
ruVmStateUpdateWorkerCodeOffset :: RuVmState -> Int -> Either RuException RuVmState
ruVmStateUpdateWorkerCodeOffset vm offset = Right vm

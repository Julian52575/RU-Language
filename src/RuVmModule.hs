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

data RuVmVariables = RuVmVariables {
    variableStack :: [RuVariable],
    tmpVariable :: RuVariable,
    returnVariable :: RuVariable,
    argumentVariables :: [RuVariable],
    globalVariables :: [RuVariable],
    carry :: Bool
} deriving (Eq, Show)

defaultRuVmVariables :: RuVmVariables
defaultRuVmVariables = RuVmVariables {
    variableStack = [],
    tmpVariable = defaultRuVariable,
    returnVariable = defaultRuVariable,
    argumentVariables = [],
    globalVariables = [],
    carry = False
}

data RuVmState = RuVmState {
    variables :: RuVmVariables, 
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8],
    conditionalMode :: Bool,
    scopeDeep :: Int
} deriving (Eq, Show)

checkVmStateCodeOffset :: RuVmInfo -> RuVmState -> Maybe RuException
checkVmStateCodeOffset info state
    | workerCodeOffset state > codeSize info = Just ruExceptionInvalidProgramCounter
    | otherwise = Nothing

checkVmState :: RuVmInfo -> RuVmState -> Maybe RuException
checkVmState info state = checkVmStateCodeOffset info state



ruFormatToRuVmState :: RuFormat -> Either RuException RuVmState
ruFormatToRuVmState format = do
    let codeOffsetInt = fromIntegral (codeOffset (ruHeader format))
    Right RuVmState {
        variables = defaultRuVmVariables,
        workerCodeOffset = codeOffset (ruHeader format),
        workerCode = drop codeOffsetInt (codeSection format),
        conditionalMode = False,
        scopeDeep = 0
    }

data RuVmInfo = RuVmInfo {
    stringTable :: [String], --the first string must be '\0'
    functionTable :: [RuFunctionTable],
    code :: [Word8],
    codeSize :: Word32,
    dumpMode :: Bool
    --vmState :: RuVmState -- Separate to ensure only VmState is updated ?
} deriving (Eq, Show)


convertWord8ToStringTable :: [Word8] -> String -> [String]
convertWord8ToStringTable (w:ws) currentStr
    | w == 0x00 = [ (currentStr ++ "\0") ] ++ convertWord8ToStringTable ws []
    | 0x01 <= w && w <= 0x7f = convertWord8ToStringTable ws (currentStr ++ [wCharTrue])
    | otherwise = convertWord8ToStringTable ws (currentStr ++ [wChar])
    where
        wChar = '.'
        wCharTrue = chr (fromIntegral w)
convertWord8ToStringTable _ _ = []


convertWord8ToFunctionTable :: [Word8] -> [RuFunctionTable]
convertWord8ToFunctionTable (n:a:m:e:o:f:f2:t:s:i:z:e2:next) = do
    [ fun ] ++ convertWord8ToFunctionTable next
    where
    fun = RuFunctionTable {
        nameIndex = word84ToWord32 n a m e,
        codeSectionOffset = word84ToWord32 o f f2 t,
        size = word84ToWord32 s i z e2
    }
convertWord8ToFunctionTable _ = []


{-- Assume RuFormat is valid
 --}
ruFormatToRuVmInfo :: RuFormat -> RuVmInfo
ruFormatToRuVmInfo format = RuVmInfo {
    stringTable = convertWord8ToStringTable (strTab format) [],
    functionTable = convertWord8ToFunctionTable (ruFunctionTable format),
    code = codeSection format,
    codeSize = codSiz,
    dumpMode = False
}
    where
        codSiz = (fileSize (ruHeader format)) - (codeOffset (ruHeader format))

{-- Get a RuVm from a fileName
 --}
fileNameToRuVm :: String -> IO (Either RuException RuVmInfo)
fileNameToRuVm fileName = runExceptT $ do --lire fichier
    byteString <- liftIO $ BS.readFile fileName
    let byteList = BS.unpack byteString
    let result = RF.fileContentToRuFormat byteList
    case result of
        Left err -> throwE err
        Right format -> do
            case ruFormatIsValid format of
                Left err -> throwE err
                Right validFormat -> return (ruFormatToRuVmInfo validFormat)



{-- Helper function to update program counter
 --}
 --
ruVmStateCheckOutOfBound :: RuVmInfo -> RuVmState -> Bool
ruVmStateCheckOutOfBound info state
    | (workerCodeOffset state) >= (codeSize info) = True
    | otherwise = False

ruVmStateUpdateWorkerCodeToPc :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruVmStateUpdateWorkerCodeToPc info state
    | ruVmStateCheckOutOfBound info state = Left ruExceptionInvalidProgramCounter
    | otherwise = Right movedState
    where
        offsetInt = fromIntegral (workerCodeOffset state)
        movedState = state {
            workerCode = drop offsetInt (code info)
        }

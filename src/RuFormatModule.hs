module RuFormatModule where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits
import Data.Either

import qualified Data.ByteString as BS

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import RuExceptionModule

-- fromIntegral
word82ToWord16 :: Word8 -> Word8 -> Word16
word82ToWord16 a b = (x `shiftL` 8) + y
    where
    x = fromIntegral a
    y = fromIntegral b

word84ToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word84ToWord32 a b c d = (x `shiftL` 24) + (y `shiftL` 16) + (z `shiftL` 8) + v
    where
    x = fromIntegral a
    y = fromIntegral b
    z = fromIntegral c
    v = fromIntegral d

data RuHeader = RuHeader {
    fileSize :: Word32,
    fileVersion :: Word8,
    functionTableCount :: Word32,
    strTableOffset :: Word32,
    strTableCount :: Word32,
    codeOffset :: Word32,
    entrypointOffset :: Word32
} deriving(Eq, Show)

data RuFunctionTable = RuFunctionTable {
    nameIndex :: Word32,
    codeSectionOffset :: Word32,
    size :: Word32
} deriving (Eq, Show)

data RuFormat = RuFormat {
    ruHeader :: RuHeader,
    ruFunctionTable :: [Word8],
    strTab :: [Word8],
    codeSection :: [Word8]
} deriving (Eq, Show)

defaultRuFormat :: RuFormat
defaultRuFormat = RuFormat {
        ruHeader = RuHeader {
            fileSize = 0x00,
            fileVersion = 0x00,
            functionTableCount = 0x00,
            strTableOffset = 0x00,
            strTableCount = 0x00,
            codeOffset = 0x00,
            entrypointOffset = 0x00
        },
    ruFunctionTable = [],
    strTab = [],
    codeSection = []
}

{--
 --}
ruMagic :: [Word8]
ruMagic = [0x43, 0x52, 0x4f, 0x55, 0x53]

checkMagic :: RuFormat -> [Word8] -> Either RuException RuFormat
checkMagic format (m:a:g:i:c:next)
    | [m, a, g, i, c] /= ruMagic = Left ruExceptionWrongFileFormat
    | otherwise = checkChecksumFileVersion format next
checkMagic _ _ = Left ruExceptionGenericFileError

{--
 --}
getFileChecksum :: [Word8] -> Word16 -> Word16
getFileChecksum (x:xs) value = getFileChecksum xs (value + a)
    where
        a = fromIntegral x
getFileChecksum [] value = value

checkChecksumFileVersion :: RuFormat -> [Word8] -> Either RuException RuFormat
checkChecksumFileVersion format (_:_:version:next)
    | version /= 0x01 = Left ruExceptionUnsupportedVersion
    | otherwise       = checkFunctionTableCount newFormat next
    where
        newFormat = format {
            ruHeader = (ruHeader format) {
                fileVersion = version
            }
        }
checkChecksumFileVersion _ _ = Left ruExceptionGenericFileError

{--
 --}
checkFunctionTableCount :: RuFormat -> [Word8] -> Either RuException RuFormat
checkFunctionTableCount format (a:b:c:d:next) = do
    let count = (word84ToWord32 a b c d)
    let newFormat = format {
            ruHeader = (ruHeader format) {
                functionTableCount = count
            }
        }
    if count < 0 then Left ruExceptionBadFunctionTableCount
    else checkStrTableOffset newFormat next 
checkFunctionTableCount _ _ = Left ruExceptionGenericFileError


checkStrTableOffset :: RuFormat -> [Word8] -> Either RuException RuFormat
checkStrTableOffset format (a:b:c:d:next) = do
    let offset = (word84ToWord32 a b c d)
    let newFormat = format {
            ruHeader = (ruHeader format) {
                strTableOffset = offset
            }
        }
    if offset < 0 then Left ruExceptionBadFunctionTableCount
    else checkStrTableCount newFormat next
checkStrTableOffset _ _ = Left ruExceptionGenericFileError

checkStrTableCount :: RuFormat -> [Word8] -> Either RuException RuFormat
checkStrTableCount format (a:b:c:d:next) = do
    let count = (word84ToWord32 a b c d)
    let newFormat = format {
            ruHeader = (ruHeader format) {
                strTableCount = count
            }
        }
    if count < 0 then Left ruExceptionBadFunctionTableCount
    else checkCodeOffset newFormat next
checkStrTableCount _ _ = Left ruExceptionGenericFileError


checkCodeOffset :: RuFormat -> [Word8] -> Either RuException RuFormat
checkCodeOffset format (a:b:c:d:next) = do
    let offset = (word84ToWord32 a b c d)
    let newFormat = format {
            ruHeader = (ruHeader format) {
                codeOffset = offset
            }
        }
    if offset < 0 then Left ruExceptionBadFunctionTableCount
    else checkEntrypointOffset newFormat next
checkCodeOffset _ _ = Left ruExceptionGenericFileError


checkEntrypointOffset :: RuFormat -> [Word8] -> Either RuException RuFormat
checkEntrypointOffset format (a:b:c:d:_) = do
    let offset = (word84ToWord32 a b c d)
    let newFormat = format {
            ruHeader = (ruHeader format) {
                entrypointOffset = offset
            }
        }
    if offset < 0 then Left ruExceptionBadFunctionTableCount
    else Right newFormat
checkEntrypointOffset _ _ = Left ruExceptionGenericFileError


{-- Helper function to extract file field
 --}
fileContentToRuFormat :: [Word8] -> Either RuException RuFormat
fileContentToRuFormat tab = do
    let format = defaultRuFormat
    let result = checkMagic format tab
    if (isLeft result == True) then result
    else do
        let newFormat = defaultRuFormat
        let header = ruHeader (fromRight newFormat result)
        -- let functionTable = take (fromIntegral (functionTableCount header)) (drop ((fromIntegral (strTableOffset header)) - 1) tab)
        let functionTable = take (12 * (fromIntegral(functionTableCount header))) (drop 64 tab)
        let strTable = take ((fromIntegral (codeOffset header)) - (fromIntegral (strTableOffset header))) (drop (fromIntegral (strTableOffset header)) tab)
        let codSection = drop (fromIntegral (codeOffset header)) tab
        Right newFormat {
            ruHeader = header,
            ruFunctionTable = functionTable,
            strTab = strTable,
            codeSection = codSection
        }

{-- Get a RuFormat from a fileName
--}
fileNameToRuFormat :: String -> IO (Either RuException RuFormat)
fileNameToRuFormat fileName = runExceptT $ do
    byteString <- liftIO $ BS.readFile fileName
    let byteList = BS.unpack byteString
    let result = fileContentToRuFormat byteList
    if isLeft result then throwE (fromLeft ruExceptionGenericFileError result)
    else return (fromRight (error "Unexpected error: result should be Right") result)

-- fileNameToRuFormat :: String -> IO (Either RuException RuFormat)
-- fileNameToRuFormat fileName = runExceptT $ do
--     byteString <- liftIO $ BS.readFile fileName
--     let byteList = BS.unpack byteString
--     let result = fileContentToRuFormat byteList
--     if (isLeft result == True) then throwE (fromLeft ruExceptionGenericFileError result)
--     else return $ (fromRight defaultRuFormat result)
--
ruFormatIsValid :: RuFormat -> Either RuException RuFormat
ruFormatIsValid format = Left ruExceptionGenericFileError --TODO

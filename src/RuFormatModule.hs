module RuFormatModule where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits
import Data.Either

import qualified Data.ByteString as BS

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

{--
 --}
getFileChecksum :: [Word8] -> Word16 -> Word16
getFileChecksum (x:xs) value = getFileChecksum xs (value + a)
    where
        a = fromIntegral x
getFileChecksum [] value = value

checkChecksumFileVersion :: RuFormat -> [Word8] -> Either RuException RuFormat
checkChecksumFileVersion format (check:sum:version:next) = do
    let calculatedChecksum = (getFileChecksum next 0)
    let fileChecksum = (word82ToWord16 check sum)
    --if calculatedChecksum != fileChecksum then Left ruExceptionBadChecksum
    if version /= 0x01 then Left ruExceptionUnsupportedVersion
    else checkFunctionTableCount newFormat next
    where
        newFormat = format {
            ruHeader = (ruHeader format) {
                fileVersion = version
            }
        }

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


checkEntrypointOffset :: RuFormat -> [Word8] -> Either RuException RuFormat
checkEntrypointOffset format (a:b:c:d:next) = do
    let offset = (word84ToWord32 a b c d)
    let newFormat = format {
            ruHeader = (ruHeader format) {
                entrypointOffset = offset
            }
        }
    if offset < 0 then Left ruExceptionBadFunctionTableCount
    else Right newFormat


{-- Helper function to extract file field
 --}
fileContentToRuFormat :: [Word8] -> Either RuException RuFormat
fileContentToRuFormat tab = do
    let format = defaultRuFormat
    let result = checkMagic format tab
    if (isLeft result == True) then result
    else Right format

{-- Get a RuFormat from a fileName
fileNameToRuFormat :: String -> Either RuException RuFormat
fileNameToRuFormat fileName = do
    byteString <- BS.readFile fileName
    let byteList = BS.unpack byteString
    let result = fileContentToRuFormat byteList
    if (isLeft result == True) then result
    else Right (fromLeft defaultRuFormat result)

    --}

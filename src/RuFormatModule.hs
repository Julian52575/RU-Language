module RuFormatModule where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits
import Data.Either

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
    fileVersion :: Word8,
    functionTableCount :: Word32,
    strTableOffset :: Word32,
    strTableCount :: Word32,
    codeOffset :: Word32,
    entrypointOffset :: Word32
} deriving(Eq, Show)

data RuFunctionTable = RuFunctionTable {
    nameIndex :: Word64,
    codeSectionOffset :: Word64,
    size :: Word64
} deriving (Eq, Show)

data RuFormat = RuFormat {
    ruHeader :: RuHeader,
    ruFunctionTable :: [RuFunctionTable],
    strTab :: [String],
    codeSection :: [Word8]
}

defaultRuFormat :: RuFormat
defaultRuFormat = RuFormat {
    ruHeader = RuHeader {
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
            ruHeader = RuHeader {
                fileVersion = version
            }
        }

{--
 --}
checkFunctionTableCount :: RuFormat -> [Word8] -> Either RuException RuFormat
checkFunctionTableCount format _ = Left (RuException "Todo")

{-- Helper function to extract file field
 --}
fileContentToRuFormat :: [Word8] -> Either RuException RuFormat
fileContentToRuFormat tab = do
    let format = defaultRuFormat
    let result = checkMagic format tab
    if (isLeft result == True) then result
    else Left (RuException "Todo")

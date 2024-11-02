module Compiler.Header (
    getHeader,
    headerToByteString,
    opListCountByte,
    opCodeToByteString
) where

import Compiler.Type (OpCode(..), CodingByte(..), Header(..), Compile(..), Function(..))
import Data.Word (Word8, Word16, Word32)
import qualified Data.ByteString as B
import Data.Bits (shiftR)
import Data.List (replicate, mapAccumL)
import Data.Maybe (fromMaybe)

intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

intToWord32List :: Int -> [Word8]
intToWord32List n = [fromIntegral (shiftR n 24), fromIntegral (shiftR n 16), fromIntegral (shiftR n 8), fromIntegral n]

codingByteToByte :: CodingByte -> [Word8]
codingByteToByte (CbConst a b c) = [fromIntegral a] ++ intToWord32List b ++ intToWord32List c
codingByteToByte (CbVar a b) = intToWord32List a ++ intToWord32List b

addCodingByteToByte :: CodingByte -> CodingByte -> [Word8]
addCodingByteToByte (CbConst a b c) (CbConst x y z) =
    case a of
        0xA0 -> case x of
            0xA0 -> [fromIntegral 0xAA] ++ intToWord32List b ++ intToWord32List c ++ intToWord32List y ++ intToWord32List z
            _ -> [fromIntegral 0xAB] ++ intToWord32List b ++ intToWord32List c ++ intToWord32List y ++ intToWord32List z
        0xB0 -> case x of
            0xA0 -> [fromIntegral 0xBA] ++ intToWord32List b ++ intToWord32List c ++ intToWord32List y ++ intToWord32List z
            _ -> [fromIntegral 0xBB] ++ intToWord32List b ++ intToWord32List c ++ intToWord32List y ++ intToWord32List z
addCodingByteToByte (CbVar a b) (CbVar x y) = intToWord32List a ++ intToWord32List b ++ intToWord32List x ++ intToWord32List y


opCodeToByte :: OpCode -> [Word8]
opCodeToByte OpNoop = [0x00, 0x00]
opCodeToByte (OpPrint cb) = 0x00 : [0x01] ++ codingByteToByte cb
opCodeToByte (OpPrintLn cb) = 0x00 : [0x02] ++ codingByteToByte cb

opCodeToByte (OpCreateVar a b) = 0x01 : [0x00] ++ intToWord32List a ++ intToWord32List b
opCodeToByte (OpSetVar a cb) = 0x01 : [0x01] ++ addCodingByteToByte (CbConst 0xA0 0x01 a) cb
opCodeToByte (OpSetTmp a cb) = 0x01 : [0x02] ++ codingByteToByte cb
opCodeToByte (OpSetArg a cb) = 0x01 : [0x03]  ++ addCodingByteToByte (CbConst 0xA0 0x01 a) cb
opCodeToByte (OpUnsetArg a b) = 0x01 : [0x04] ++ intToWord32List a ++ intToWord32List b
opCodeToByte (OpSetReturn a cb) = 0x01 : [0x05] ++ codingByteToByte cb
opCodeToByte (OpUnsetReturn a) = 0x01 : [0x06] ++ intToWord32List a
opCodeToByte (OpUnsetVar a) = 0x01 : [0x07] ++ intToWord32List a

opCodeToByte OpReturn = [0x02, 0x00]
opCodeToByte (OpCall a) = 0x02 : [0x01] ++ intToWord32List a
opCodeToByte (OpJump a) = 0x02 : [0x02] ++ intToWord32List a
opCodeToByte (OpJumpCarry a) = 0x02 : [0x03] ++ intToWord32List a
opCodeToByte (OpJumpNotCarry a) = 0x02 : [0x04] ++ intToWord32List a

opCodeToByte (OpAdd cb1 cb2) = 0x03 : [0x00] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpSub cb1 cb2) = 0x03 : [0x01] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpDiv cb1 cb2) = 0x03 : [0x02] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpMul cb1 cb2) = 0x03 : [0x03] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpEq cb1 cb2) = 0x03 : [0x04] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpNeq cb1 cb2) = 0x03 : [0x05] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpMod cb1 cb2) = 0x03 : [0x06] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpLesser cb1 cb2) = 0x03 : [0x07] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpLesserEq cb1 cb2) = 0x03 : [0x08] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpGreater cb1 cb2) = 0x03 : [0x09] ++ addCodingByteToByte cb1 cb2
opCodeToByte (OpGreaterEq cb1 cb2) = 0x03 : [0x10] ++ addCodingByteToByte cb1 cb2

opCountCodingByte :: CodingByte -> Int
opCountCodingByte (CbConst _ _ _) = 12
opCountCodingByte (CbVar _ _) = 8

opCountByte :: OpCode -> Int
opCountByte OpNoop = 2
opCountByte (OpPrint cb) = 2 + opCountCodingByte cb
opCountByte (OpPrintLn cb) = 2 + opCountCodingByte cb
opCountByte (OpCreateVar _ _) = 10
opCountByte (OpSetVar _ cb) = 6 + opCountCodingByte cb
opCountByte (OpSetTmp _ cb) = 2 + opCountCodingByte cb
opCountByte (OpSetArg _ cb) = 6 + opCountCodingByte cb
opCountByte (OpUnsetArg _ _) = 10
opCountByte (OpSetReturn _ cb) = 2 + opCountCodingByte cb
opCountByte (OpUnsetReturn _) = 6
opCountByte (OpUnsetVar _) = 6
opCountByte OpReturn = 2
opCountByte (OpCall _) = 6
opCountByte (OpJump _) = 6
opCountByte (OpJumpCarry _) = 6
opCountByte (OpJumpNotCarry _) = 6
opCountByte (OpAdd cb1 cb2) = 18
opCountByte (OpSub cb1 cb2) = 18
opCountByte (OpDiv cb1 cb2) = 18
opCountByte (OpMul cb1 cb2) = 18
opCountByte (OpEq cb1 cb2) = 18
opCountByte (OpNeq cb1 cb2) = 18
opCountByte (OpMod cb1 cb2) = 18
opCountByte (OpLesser cb1 cb2) = 18
opCountByte (OpLesserEq cb1 cb2) = 18
opCountByte (OpGreater cb1 cb2) = 18
opCountByte (OpGreaterEq cb1 cb2) = 18

opListCountByte :: [OpCode] -> Int
opListCountByte = sum . map opCountByte

opCodeToByteString :: [OpCode] -> [Word8]
opCodeToByteString [] = []
opCodeToByteString (x:xs) = opCodeToByte x ++ opCodeToByteString xs

stringToHex :: String -> [Word8]
stringToHex str = map (\x -> fromIntegral $ fromEnum x) str ++ [0x00]

stringTableToHex :: [String] -> [Word8]
stringTableToHex [] = []
stringTableToHex (x:xs) = stringToHex x ++ stringTableToHex xs

headerToByteString :: Header -> B.ByteString
headerToByteString (Header magic checkSum version functionTableCount stringTableOffset stringTableSize codeOffset firstInstructionOffset unused stringTableHex hFunctionTable) =
    let magic' = magic ++ replicate (5 - length magic) 0x00
        checkSum' = reverse [fromIntegral checkSum, fromIntegral $ shiftR checkSum 8]
        version' = reverse [fromIntegral version]
        functionTableCount' = reverse [fromIntegral functionTableCount, fromIntegral $ shiftR functionTableCount 8, fromIntegral $ shiftR functionTableCount 16, fromIntegral $ shiftR functionTableCount 24]
        stringTableOffset' = reverse [fromIntegral stringTableOffset, fromIntegral $ shiftR stringTableOffset 8, fromIntegral $ shiftR stringTableOffset 16, fromIntegral $ shiftR stringTableOffset 24]
        stringTableSize' = reverse [fromIntegral stringTableSize, fromIntegral $ shiftR stringTableSize 8, fromIntegral $ shiftR stringTableSize 16, fromIntegral $ shiftR stringTableSize 24]
        codeOffset' = reverse [fromIntegral codeOffset, fromIntegral $ shiftR codeOffset 8, fromIntegral $ shiftR codeOffset 16, fromIntegral $ shiftR codeOffset 24]
        firstInstructionOffset' = reverse [fromIntegral firstInstructionOffset, fromIntegral $ shiftR firstInstructionOffset 8, fromIntegral $ shiftR firstInstructionOffset 16, fromIntegral $ shiftR firstInstructionOffset 24]

    in B.pack $ magic' ++ checkSum' ++ version' ++ functionTableCount' ++ stringTableOffset' ++ stringTableSize' ++ codeOffset' ++ firstInstructionOffset' ++ unused ++ hFunctionTable ++ stringTableHex

updateFunction :: Function -> [OpCode] -> Function
updateFunction (Function index name offset size) opcodes = Function index name offset (Just $ sum $ map opCountByte opcodes)

getFunctionOffset :: [Function] -> Int
getFunctionOffset [] = 0
getFunctionOffset (x:xs) = case fOffset x of
    Just offset -> offset + getFunctionOffset xs
    Nothing -> getFunctionOffset xs

updateFunctionOffsets :: [Function] -> [Function]
updateFunctionOffsets = snd . mapAccumL updateOffset 0
  where
    updateOffset acc func = (acc + fromMaybe 0 (fSize func), func { fOffset = Just acc })

addGlobalOffset :: Int -> [Function] -> [Function]
addGlobalOffset _ [] = []
addGlobalOffset offset (Function index name (Just offset') size : xs) = Function index name (Just $ offset + offset') size : addGlobalOffset offset xs

intToWord32ListFunction :: Int -> [Word8]
intToWord32ListFunction n = [fromIntegral (shiftR n 24), fromIntegral (shiftR n 16), fromIntegral (shiftR n 8), fromIntegral n]

functionToByte :: Function -> [Word8]
functionToByte (Function index _ offset size) =
    let indexBytes = intToWord32ListFunction index
        offsetBytes = intToWord32ListFunction $ fromMaybe 0 offset
        sizeBytes = intToWord32ListFunction $ fromMaybe 0 size
    in indexBytes ++ offsetBytes ++ sizeBytes

getHeader :: Compile -> [OpCode] -> [[OpCode]] -> Header
getHeader comp globalOp opcodes =
    let magic = [0x43, 0x52, 0x4F, 0x55, 0x53]
        checkSum = 0x0000
        version = 0x01
        functionTableCount = length $ functionTable comp
        stringTableOffset = 64 + (functionTableCount * 12)
        stringTableSize = length $ stringTable comp
        stringTableHex = stringTableToHex $ stringTable comp
        codeOffset = stringTableOffset + length stringTableHex
        firstInstructionOffset = 0x00
        unuse = replicate 36 0x00
        functionTableCount' = intToWord32 functionTableCount
        stringTableOffset' = intToWord32 stringTableOffset
        stringTableSize' = intToWord32 stringTableSize
        globaloffset = opListCountByte globalOp
        codeOffset' = intToWord32 codeOffset
        functionTable' = zipWith updateFunction (functionTable comp) opcodes
        functionTable'' = updateFunctionOffsets functionTable'
        functionTable''' = addGlobalOffset globaloffset functionTable''
        hFunctionTable = concatMap functionToByte functionTable'''

    in Header magic checkSum version functionTableCount' stringTableOffset' stringTableSize' codeOffset' firstInstructionOffset unuse stringTableHex hFunctionTable

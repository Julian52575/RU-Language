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
intToWord32List a = [fromIntegral a, fromIntegral $ shiftR a 8, fromIntegral $ shiftR a 16, fromIntegral $ shiftR a 24]

codingByteToByte :: CodingByte -> [Word8]
codingByteToByte (CbConst a b c) = intToWord32List a ++ intToWord32List b ++ intToWord32List c
codingByteToByte (CbVar a b) = intToWord32List a ++ intToWord32List b

opCodeToByte :: OpCode -> [Word8]
opCodeToByte OpNoop = [0x00, 0x00]
opCodeToByte (OpPrint cb) = 0x00 : [0x01] ++ codingByteToByte cb
opCodeToByte (OpPrintLn cb) = 0x00 : [0x02] ++ codingByteToByte cb

opCodeToByte (OpCreateVar a b) = 0x01 : [0x00] ++ intToWord32List a ++ intToWord32List b
opCodeToByte (OpSetVar a cb) = 0x01 : [0x01] ++ intToWord32List a ++ codingByteToByte cb
opCodeToByte (OpSetTmp a cb) = 0x01 : [0x02] ++ intToWord32List a ++ codingByteToByte cb
opCodeToByte (OpSetArg a cb) = 0x01 : [0x03] ++ intToWord32List a ++ codingByteToByte cb
opCodeToByte (OpUnsetArg a b) = 0x01 : [0x04] ++ intToWord32List a ++ intToWord32List b
opCodeToByte (OpSetReturn cb) = 0x01 : [0x05] ++ codingByteToByte cb
opCodeToByte (OpUnsetReturn a) = 0x01 : [0x06] ++ intToWord32List a

opCodeToByte OpReturn = [0x02, 0x00]
opCodeToByte (OpCall a) = 0x02 : [0x01] ++ intToWord32List a
opCodeToByte (OpJump a) = 0x02 : [0x02] ++ intToWord32List a
opCodeToByte (OpJumpCarry a) = 0x02 : [0x03] ++ intToWord32List a
opCodeToByte (OpJumpNotCarry a) = 0x02 : [0x04] ++ intToWord32List a
opCodeToByte (OpIfCarry a) = 0x02 : [0x05] ++ intToWord32List a

opCodeToByte (OpAdd cb1 cb2) = 0x03 : [0x00] ++ codingByteToByte cb1 ++ codingByteToByte cb2
opCodeToByte (OpSub cb1 cb2) = 0x03 : [0x01] ++ codingByteToByte cb1 ++ codingByteToByte cb2
opCodeToByte (OpDiv cb1 cb2) = 0x03 : [0x02] ++ codingByteToByte cb1 ++ codingByteToByte cb2
opCodeToByte (OpMul cb1 cb2) = 0x03 : [0x03] ++ codingByteToByte cb1 ++ codingByteToByte cb2
opCodeToByte (OpEq cb1 cb2) = 0x03 : [0x04] ++ codingByteToByte cb1 ++ codingByteToByte cb2
opCodeToByte (OpNeq cb1 cb2) = 0x03 : [0x05] ++ codingByteToByte cb1 ++ codingByteToByte cb2
opCodeToByte (OpMod cb1 cb2) = 0x03 : [0x06] ++ codingByteToByte cb1 ++ codingByteToByte cb2

opCountCodingByte :: CodingByte -> Int
opCountCodingByte (CbConst _ _ _) = 12
opCountCodingByte (CbVar _ _) = 8

opCountByte :: OpCode -> Int
opCountByte OpNoop = 2
opCountByte (OpPrint cb) = 2 + opCountCodingByte cb
opCountByte (OpPrintLn cb) = 2 + opCountCodingByte cb
opCountByte (OpCreateVar _ _) = 10
opCountByte (OpSetVar _ cb) = 3 + opCountCodingByte cb
opCountByte (OpSetTmp _ cb) = 3 + opCountCodingByte cb
opCountByte (OpSetArg _ cb) = 3 + opCountCodingByte cb
opCountByte (OpUnsetArg _ _) = 10
opCountByte (OpSetReturn cb) = 2 + opCountCodingByte cb
opCountByte (OpUnsetReturn _) = 7
opCountByte OpReturn = 2
opCountByte (OpCall _) = 7
opCountByte (OpJump _) = 7
opCountByte (OpJumpCarry _) = 7
opCountByte (OpJumpNotCarry _) = 7
opCountByte (OpIfCarry _) = 7
opCountByte (OpAdd cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2
opCountByte (OpSub cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2
opCountByte (OpDiv cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2
opCountByte (OpMul cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2
opCountByte (OpEq cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2
opCountByte (OpNeq cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2
opCountByte (OpMod cb1 cb2) = 2 + opCountCodingByte cb1 + opCountCodingByte cb2

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
headerToByteString (Header magic checkSum version functionTableCount stringTableOffset stringTableSize codeOffset firstInstructionOffset unused stringTableHex hFunctionTable) = do
    let magic' = magic ++ replicate (5 - length magic) 0x00
    let checkSum' = [fromIntegral checkSum, fromIntegral $ shiftR checkSum 8]
    let version' = [fromIntegral version]
    let functionTableCount' = [fromIntegral functionTableCount, fromIntegral $ shiftR functionTableCount 8, fromIntegral $ shiftR functionTableCount 16, fromIntegral $ shiftR functionTableCount 24]
    let stringTableOffset' = [fromIntegral stringTableOffset, fromIntegral $ shiftR stringTableOffset 8, fromIntegral $ shiftR stringTableOffset 16, fromIntegral $ shiftR stringTableOffset 24]
    let stringTableSize' = [fromIntegral stringTableSize, fromIntegral $ shiftR stringTableSize 8, fromIntegral $ shiftR stringTableSize 16, fromIntegral $ shiftR stringTableSize 24]
    let codeOffset' = [fromIntegral codeOffset, fromIntegral $ shiftR codeOffset 8, fromIntegral $ shiftR codeOffset 16, fromIntegral $ shiftR codeOffset 24]
    let firstInstructionOffset' = [fromIntegral firstInstructionOffset, fromIntegral $ shiftR firstInstructionOffset 8, fromIntegral $ shiftR firstInstructionOffset 16, fromIntegral $ shiftR firstInstructionOffset 24]

    B.pack $ magic' ++ checkSum' ++ version' ++ functionTableCount' ++ stringTableOffset' ++ stringTableSize' ++ codeOffset' ++ firstInstructionOffset' ++ unused ++ hFunctionTable ++ stringTableHex

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

functionToByte :: Function -> [Word8]
functionToByte (Function index _ offset size) = [fromIntegral index] ++ [fromIntegral $ fromMaybe 0 offset] ++ [fromIntegral $ fromMaybe 0 size]

getHeader :: Compile -> [OpCode] -> [[OpCode]] -> Header
getHeader comp globalOp opcodes = do
    let magic = [0x43, 0x52, 0x4F, 0x55, 0x53]
    let checkSum = 0x0000
    let version = 0x01
    let functionTableCount = length $ functionTable comp
    let stringTableOffset = 64 + (functionTableCount * 12)
    let stringTableSize = length $ stringTable comp
    let stringTableHex = stringTableToHex $ stringTable comp
    let codeOffset = stringTableOffset + length stringTableHex
    let firstInstructionOffset = 0x00
    let unuse = replicate 36 0x00
    let functionTableCount' = intToWord32 functionTableCount
    let stringTableOffset' = intToWord32 stringTableOffset
    let stringTableSize' = intToWord32 stringTableSize
    let globaloffset = opListCountByte globalOp
    let codeOffset' = intToWord32 codeOffset
    let functionTable' = zipWith updateFunction (functionTable comp) opcodes
    let functionTable'' = updateFunctionOffsets functionTable'
    let functionTable''' = addGlobalOffset globaloffset functionTable''
    let hFunctionTable = concatMap functionToByte functionTable'''

    Header magic checkSum version functionTableCount' stringTableOffset' stringTableSize' codeOffset' firstInstructionOffset unuse stringTableHex hFunctionTable

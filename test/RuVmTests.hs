module RuVmTests where

import Test.Hspec
import Data.Either

import RuExceptionModule
import RuFormatModule
import RuVmModule
{--
 - data RuVmState = RuVmState {
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
--}

spec :: Spec
spec = do
    describe "Convert RuFormat to RuVmState" $ do
        let header = RuHeader {
            fileSize = 0x00,
            fileVersion = 0x01,
            functionTableCount = 0x01,
            strTableOffset = 0x42,
            strTableCount = 0x01,
            codeOffset = 0x42,
            entrypointOffset = 0x02
        }
        let funTab = [ 0x11, 0x11, 0x11, 0x11, 0x22, 0x22, 0x22, 0x22, 0x33, 0x33, 0x33, 0x33]
        let strTabl = [ 0x00, 0x65, 0x00]
        let codeSec = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a]
        let format = RuFormat {
                ruHeader = header,
                ruFunctionTable = funTab,
                strTab = strTabl,
                codeSection = codeSec
        }
        let result = ruFormatToRuVmState format
        let state = fromRight (error "fromRight error" ) result
        it "Convert RuFormat to RuVmState" $ do
            isLeft result `shouldBe` False
        it "Parse entrypointOffset" $ do
            workerCodeOffset state `shouldBe` codeOffset (ruHeader format)
        it "Parse code" $ do
            let codeOffsetInt = fromIntegral (codeOffset (ruHeader format))
            workerCode state `shouldBe` drop codeOffsetInt (codeSection format)

    describe "Convert RuFormat to RuVmInfo" $ do
        let funSectionBase = [0xa1, 0xa2, 0xa3, 0xa4, 0xb1, 0xb2, 0xb3, 0xb4, 0xc1, 0xc2, 0xc3, 0xc4]
        let funSection = funSectionBase ++ funSectionBase
        let stringSection = [0x00, 0x61, 0x62, 0x63, 0x00] -- A B C
        let tabCodeSection = [0x01, 0x01, 0x01]
        let ffileSize = fromIntegral $ 64 + (length funSection) + (length stringSection) + (length tabCodeSection)
        let format = RuFormat {
            ruHeader = RuHeader {
                fileSize = ffileSize,
                fileVersion = 0x01,
                functionTableCount = 0x02,
                strTableOffset = fromIntegral $ 64 + (length funSection),
                strTableCount = 1,
                codeOffset = fromIntegral $ 64 + (length funSection) + (length stringSection),
                entrypointOffset = 0x00
            },
            ruFunctionTable = funSection,
            strTab = stringSection,
            codeSection = tabCodeSection
        }
        --ruFormatToRuVmInfo :: RuFormat -> Either RuException RuVmInfo
        let expectedFunction = RuFunctionTable {
                                    nameIndex = 0xa1a2a3a4,
                                    codeSectionOffset = 0xb1b2b3b4,
                                    size = 0xc1c2c3c4
                                   }
        let vm = ruFormatToRuVmInfo format
        it "Convert String Table" $ do
            stringTable vm `shouldBe` [ "\0", "abc\0" ]
        it "Convert Function Table" $ do
            functionTable vm `shouldBe` [ expectedFunction, expectedFunction ]
        it "Convert Code" $ do
            code vm `shouldBe` tabCodeSection
            codeSize vm `shouldBe` 0x03

        --convertWord8ToStringTable :: [Word8] -> String -> [String]
        it "Convert [Word8] into a string array" $ do
            let tab = [0x00, 0x65, 0x00, 0x65, 0x065, 0x00, 0x65, 0x65, 0x65, 0x00]
            let stringTab = convertWord8ToStringTable tab ""
            stringTab `shouldBe` [ "\0", "e\0", "ee\0", "eee\0" ]

        --convertWord8ToFunctionTable :: [Word8] -> [ruFunctionTable]
        it "Convert [Word8] into function table" $ do
            let tab = [0xa1, 0xa2, 0xa3, 0xa4, 0xb1, 0xb2, 0xb3, 0xb4, 0xc1, 0xc2, 0xc3, 0xc4]
            let expected = RuFunctionTable {
                nameIndex = 0xa1a2a3a4,
                codeSectionOffset = 0xb1b2b3b4,
                size = 0xc1c2c3c4
            }
            let finalTab = tab ++ tab
            let fun = convertWord8ToFunctionTable finalTab
            fun `shouldBe` [ expected, expected ] 




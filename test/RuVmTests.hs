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
    let tabCode = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05]
    {--let vm = RuVmInfo {
        stringTable = ["0"],
        functionTable = [],
        code = tabCode,
        codeSize = 0x05
    }--}
    let vmState = RuVmState {
        variableStack = [],
        workerCodeOffset = 0x00,
        workerCode = tabCode
    }
    describe "Updates Worker Code Offset / Program counter" $ do
        it "moves forward" $ do
            let result = ruVmStateUpdateWorkerCodeOffset vmState 3
            isRight result `shouldBe` True
            let newVm = fromRight (error "Unexpected Left") result
            (workerCodeOffset newVm) `shouldBe` 3
            (workerCode newVm) `shouldBe` [0x03, 0x04, 0x05]

        it "moves backward" $ do
            let fResult = ruVmStateUpdateWorkerCodeOffset vmState 3
            isRight fResult `shouldBe` True
            let fVm = fromRight (error "Unexpected Left") fResult
            let result = ruVmStateUpdateWorkerCodeOffset fVm (-2)
            isRight result `shouldBe` True
            let newVm = fromRight (error "Unexpected Left") result
            workerCodeOffset (newVm) `shouldBe` 1
            workerCode (newVm) `shouldBe` [0x01, 0x02, 0x03, 0x04, 0x05]

        it "handles out of bound negative offset" $ do
            let result = ruVmStateUpdateWorkerCodeOffset vmState (-1)
            isLeft result `shouldBe` True

        it "handles out of bound positive offset" $ do
            let oobOffset = fromIntegral ((length tabCode) + 1)
            let result = ruVmStateUpdateWorkerCodeOffset vmState oobOffset
            isLeft result `shouldBe` True

    -- ruFormatToRuVmState :: RuFormat -> Either RuException RuVmState
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
        let expected = RuVmState {
                variableStack = [],
                workerCodeOffset = 0x02,
                workerCode = (drop 0x02 codeSec),
                conditionalMode = False
        }
        let result = ruFormatToRuVmState format
        let state = fromRight (RuVmState {} ) result
        it "Convert RuFormat to RuVmState" $ do
            isLeft result `shouldBe` False
        it "Parse entrypointOffset" $ do
            workerCodeOffset state `shouldBe` codeOffset (ruHeader format)
        it "Parse code" $ do
            let codeOffsetInt = fromIntegral (codeOffset (ruHeader format))
            workerCode state `shouldBe` drop codeOffsetInt (codeSection format)

    describe "Convert RuFormat to RuVmInfo" $ do
        let funSection = [0xa1, 0xa2, 0xa3, 0xa4, 0xb1, 0xb2, 0xb3, 0xb4, 0xc1, 0xc2, 0xc3, 0xc4]
        let stringSection = [0x65, 0x66, 0x67] -- A B C
        let tabCodeSection = [0x01, 0x01, 0x01]
        let format = RuFormat {
            ruHeader = RuHeader {
                fileSize = 0x01,
                fileVersion = 0x01,
                functionTableCount = 0x01,
                strTableOffset = 64 + 12,
                strTableCount = 0x01,
                codeOffset = 64 + 12 + 5,
                entrypointOffset = 64 + 12 + 5
            },
            ruFunctionTable = funSection,
            strTab = stringSection,
            codeSection = tabCodeSection
        }
        --ruFormatToRuVmInfo :: RuFormat -> Either RuException RuVmInfo
        it "Convert RuFormat to RuVmInfo" $ do
            let expected = RuFunctionTable {
                                    nameIndex = 0xa1a2a3a4,
                                    codeSectionOffset = 0xb1b2b3b4,
                                    size = 0xc1c2c3c4
                                   }
            let result = ruFormatToRuVmInfo format
            isRight result `shouldBe` True
            let vm = fromRight ( RuVmInfo {} ) result
            stringTable vm `shouldBe` [ "abc" ]
            functionTable vm `shouldBe` [ expected, expected ]
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




module RuVmTests where

import Test.Hspec
import Data.Either

import RuVmModule
{--
 - data RuVmState = RuVmState {
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
--}

spec :: Spec
spec = do
    let tabCode = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05]
    let vm = RuVm {
        fileVersion = 0x00,
        stringTable = ["0"],
        functionTable = [],
        code = tabCode,
        codeSize = 0x05,
        ruVmState = RuVmState {
            variableStack = [],
            workerCodeOffset = 0,
            workerCode = tabCode
        }
    }
    describe "Updates Worker Code Offset / Program counter" $ do
        it "moves forward" $ do
            let result = ruVmUpdateWorkerCodeOffset vm 3
            isRight result `shouldBe` True
            let newVm = fromRight (error "Unexpected Left") result 
            (workerCodeOffset (ruVmState newVm)) `shouldBe` 3
            (workerCode (ruVmState newVm)) `shouldBe` [0x03, 0x04, 0x05]

        it "moves backward" $ do
            let fResult = ruVmUpdateWorkerCodeOffset vm 3
            isRight fResult `shouldBe` True
            let fVm = fromRight (error "Unexpected Left") fResult
            let result = ruVmUpdateWorkerCodeOffset fVm (-2)
            isRight result `shouldBe` True
            let newVm = fromRight (error "Unexpected Left") result
            (workerCodeOffset (ruVmState newVm)) `shouldBe` 1
            (workerCode (ruVmState newVm)) `shouldBe` [0x01, 0x02, 0x03, 0x04, 0x05]

        it "handles out of bound negative offset" $ do
            let result = ruVmUpdateWorkerCodeOffset vm (-1)
            isLeft result `shouldBe` True

        it "handles out of bound positive offset" $ do
            let oobOffset = fromIntegral ((codeSize vm) + 1)
            let result = ruVmUpdateWorkerCodeOffset vm oobOffset
            isLeft result `shouldBe` True


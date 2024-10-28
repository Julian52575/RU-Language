module RuInstructionsTests where

import Test.Hspec
import qualified Data.ByteString as B
import Data.Either

import RuVmModule
import RuInstructionsModule
import RuInstructionsHelperModule

{--
 data RuVmState = RuVmState {
    variables :: RuVmVariables, 
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8],
    conditionalMode :: Bool,
    scopeDeep :: Int
} deriving (Eq, Show)
--}

spec :: Spec
spec = do
    let baseInfo = RuVmInfo {
        stringTable = ["\0", "lol"],
        functionTable = [],
        code = [],
        codeSize = 0xff,
        dumpMode = False
    }
    let baseState = RuVmState {
        variables = defaultRuVmVariables,
        workerCodeOffset = 0x00,
        workerCode = [0xff, 0x00, 0x00, 0x00, 0x01],
        conditionalMode = False,
        scopeDeep = 0,
        toPrint = []
    }
--getRuInstruction :: Word8 -> Word8 -> Maybe RuInstruction
    describe "getRuInstruction" $ do
        it "Get Noop" $ do
            case getRuInstruction 0x00 0x00 of
                Nothing -> False `shouldBe` True
                Just ins -> do
                    ruInstructionPrefix ins `shouldBe` ruInstructionPrefix ruInstructionNoop
                    ruInstructionInfix ins `shouldBe` ruInstructionInfix ruInstructionNoop
                    ruInstructionName ins `shouldBe` ruInstructionName ruInstructionNoop
                    fixedSize ins `shouldBe` fixedSize ruInstructionNoop
        it "Get PrintLn" $ do
            case getRuInstruction 0x00 0x02 of
                Nothing -> False `shouldBe` True
                Just ins -> do
                    ruInstructionPrefix ins `shouldBe` ruInstructionPrefix ruInstructionPrintLn
                    ruInstructionInfix ins `shouldBe` ruInstructionInfix ruInstructionPrintLn
                    ruInstructionName ins `shouldBe` ruInstructionName ruInstructionPrintLn
                    fixedSize ins `shouldBe` fixedSize ruInstructionPrintLn
        it "Handle unknow mnemonic" $ do
            case getRuInstruction 0xff 0xff of
                Nothing -> True `shouldBe` True
                _ -> True `shouldBe` False

    describe "Noop" $ do
        it "Noop" $ do
            case ruInstructionFunctionNoop baseInfo baseState of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` baseState

    describe "Print" $ do
        it "Print" $ do
            case ruInstructionFunctionPrint baseInfo baseState of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` baseState { toPrint = "0" }

        it "PrintLn" $ do
            case ruInstructionFunctionPrintLn baseInfo baseState of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` baseState { toPrint = "0\n" }
    

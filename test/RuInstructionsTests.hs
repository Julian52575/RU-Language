module RuInstructionsTests where

import Test.Hspec
import qualified Data.ByteString as B
import Data.Either

import RuVmModule
import RuInstructionsModule

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
    let baseState = RuVmState {
        variables = defaultRuVmVariables,
        workerCodeOffset = 0x00,
        workerCode = [0xff, 0x00, 0x00, 0x00, 0x01],
        conditionalMode = False,
        scopeDeep = 0,
        toPrint = []
    }
    describe "Noop" $ do
        it "Noop" $ do
            case ruInstructionFunctionNoop baseState of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` baseState

        it "Print" $ do
            case ruInstructionFunctionPrint baseState of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` baseState { toPrint = "0" }

        it "PrintLn" $ do
            case ruInstructionFunctionPrintLn baseState of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` baseState { toPrint = "0\n" }

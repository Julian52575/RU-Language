module RuInstructionsTests where

import Test.Hspec
import qualified Data.ByteString as B
import Data.Either

import RuVmModule
import RuInstructionsModule
import RuInstructionsHelperModule
import RuVariableModule
import RuExceptionModule

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
    describe "Vars" $ do
        it "CreateVar with int" $ do
            let state = baseState { workerCode = [0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a] }
            case ruInstructionFunctionCreateVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { variableStack = [[var]] }
                    resultState `shouldBe` state { variables = vmVar }
        it "CreateVar with str" $ do
            let state = baseState { workerCode = [0x01, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionCreateVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { variableStack = [[var]] }
                    resultState `shouldBe` state { variables = vmVar }
        it "CreateVar with too few operands" $ do
            let state = baseState { workerCode = [0x01, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionCreateVar baseInfo (state) of
                Left err -> do
                    err `shouldBe` ruExceptionIncompleteInstruction
                Right resultState -> do
                    False `shouldBe` True --Fail
        it "SetTmpVarInt" $ do
            let state = baseState { workerCode = [0x01, 0x02, 0xa0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { tmpVariable = var }
                    resultState `shouldBe` state { variables = vmVar }
        
        it "SetTmpVarStr" $ do
            let state = baseState { workerCode = [0x01, 0x02, 0xa0, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { tmpVariable = var }
                    resultState `shouldBe` state { variables = vmVar }

        it "SetTmpVar with too few operands" $ do
            let state = baseState { workerCode = [0x01, 0x02, 0xa0, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    err `shouldBe` ruExceptionIncompleteInstruction
                Right resultState -> do
                    False `shouldBe` True --Fail
        
        it "SetTmpVar with id" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState { variables = vmVar, workerCode = [0x01, 0x02, 0xb0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { tmpVariable = var }
                    resultState `shouldBe` state { variables = vmVarr }
        it "SetVar" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState { variables = vmVar, workerCode = [0x01, 0x01, 0xa0 ,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04] }
            case ruInstructionFunctionSetVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { variableStack = [[var { ruVariableValue = Int32 4 }]] }
                    resultState `shouldBe` state { variables = vmVarr }
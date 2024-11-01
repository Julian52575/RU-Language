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
        it "Print int" $ do
            let state = baseState {workerCode = [0xa0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a]}
            case ruInstructionFunctionPrint baseInfo state of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` state { toPrint = "42" }
        it "Print str" $ do
            let var = RuVariable { ruVariableValue = Str "test", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState {variables = vmVar, workerCode = [0xb0, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00]}
            case ruInstructionFunctionPrint baseInfo state of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` state { toPrint = "test" }

        it "PrintLn int" $ do
            let state = baseState {workerCode = [0xa0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a]}
            case ruInstructionFunctionPrintLn baseInfo state of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` state { toPrint = "42\n" }
        it "PrintLn str" $ do
            let var = RuVariable { ruVariableValue = Str "test", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState {variables = vmVar, workerCode = [0xb0, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00]}
            case ruInstructionFunctionPrintLn baseInfo state of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    resultState `shouldBe` state { toPrint = "test\n" }
    describe "Vars" $ do
        it "CreateVar with int" $ do
            let state = baseState { workerCode = [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a] }
            case ruInstructionFunctionCreateVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { variableStack = [[var]] }
                    resultState `shouldBe` state { variables = vmVar }
        it "CreateVar with str" $ do
            let state = baseState { workerCode = [0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionCreateVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { variableStack = [[var]] }
                    resultState `shouldBe` state { variables = vmVar }
        it "CreateVar with too few operands" $ do
            let state = baseState { workerCode = [0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionCreateVar baseInfo (state) of
                Left err -> do
                    err `shouldBe` ruExceptionIncompleteInstruction
                Right resultState -> do
                    False `shouldBe` True --Fail
        it "SetTmpVarInt" $ do
            let state = baseState { workerCode = [0xa0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { tmpVariable = var }
                    resultState `shouldBe` state { variables = vmVar }
        
        it "SetTmpVarStr" $ do
            let state = baseState { workerCode = [0xa0, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let var = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
                    let vmVar = defaultRuVmVariables { tmpVariable = var }
                    resultState `shouldBe` state { variables = vmVar }

        it "SetTmpVar with too few operands" $ do
            let state = baseState { workerCode = [0xa0, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionSetTmpVar baseInfo (state) of
                Left err -> do
                    err `shouldBe` ruExceptionIncompleteInstruction
                Right resultState -> do
                    False `shouldBe` True --Fail
        
        it "SetTmpVar with id" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState { variables = vmVar, workerCode = [0xb0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] }
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
            let state = baseState { variables = vmVar, workerCode = [0xa0 ,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04] }
            case ruInstructionFunctionSetVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { variableStack = [[var { ruVariableValue = Int32 4 }]] }
                    resultState `shouldBe` state { variables = vmVarr }
    describe "Operations" $ do
        it "Add int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionAdd baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = RuVariable { ruVariableValue = Int32 84, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True } }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Add str" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "test", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionAdd baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = RuVariable { ruVariableValue = Str "loltest", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True } }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Sub int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbc, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionSub baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = RuVariable { ruVariableValue = Int32 0, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True } }
                    resultState `shouldBe` state { variables = vmVarr }
        it "div int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 2, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbd, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionDiv baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = RuVariable { ruVariableValue = Int32 21, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True } }
                    resultState `shouldBe` state { variables = vmVarr }
        it "div by 0" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 0, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionDiv baseInfo (state) of
                Left err -> do
                    err `shouldBe` ruExceptionDivByZero
                Right resultState -> do
                    False `shouldBe` True --Fail
        it "Mul int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 2, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionMul baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = RuVariable { ruVariableValue = Int32 84, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True } }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Equal int true" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Equal int false" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 43, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Equal str true" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Equal str false" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "lol2", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
        it "NotEqual int true" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 43, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionNeq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "NotEqual int false" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionNeq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Lesser int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 43, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionLesser baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Lesser str" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "lol2", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionLesser baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Greater int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 43, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionGreater baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
        it "Greater str" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "lol2", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionGreater baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
        it "LesserEqual int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 43, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionLesserEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "LesserEqual str" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "lol2", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionLesserEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = True }
                    resultState `shouldBe` state { variables = vmVarr }
        it "GreaterEqual int" $ do
            let var1 = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Int32 43, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionGreaterEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
        it "GreaterEqual str" $ do
            let var1 = RuVariable { ruVariableValue = Str "lol", ruVariableType = ruVariableTypeStr, ruVariableId = 0x00, ruMutable = True }
            let var2 = RuVariable { ruVariableValue = Str "lol2", ruVariableType = ruVariableTypeStr, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var1, var2]]}
            let state = baseState { variables = vmVar, workerCode = [0xbb, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionGreaterEq baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { carry = False }
                    resultState `shouldBe` state { variables = vmVarr }
    describe "Args" $ do
        it "SetArg" $ do
            let var = RuVariable {
                ruVariableValue = Int32 42,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x00,
                ruMutable = True
            }
            let vmVar = defaultRuVmVariables {
                variableStack = [[var]]
            }
            let state = baseState {
                variables = vmVar,
                workerCode = [0xac, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
            }
            case ruInstructionFunctionSetArg baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { argumentVariables = [[var]] }
                    resultState `shouldBe` state { variables = vmVarr }
        it "SetReturn" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState { variables = vmVar, workerCode = [0xb0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionSetReturn baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = var }
                    resultState `shouldBe` state { variables = vmVarr }
        it "SetReturn direct" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[]]}
            let state = baseState { variables = vmVar, workerCode = [0xa0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a] }
            case ruInstructionFunctionSetReturn baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = vmVar { returnVariable = var }
                    resultState `shouldBe` state { variables = vmVarr }
        it "UnSetReturn" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let retvar = RuVariable { ruVariableValue = Int32 84, ruVariableType = ruVariableTypeInt, ruVariableId = 0x00, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]], returnVariable = retvar }
            let state = baseState { variables = vmVar, workerCode = [0x00, 0x00, 0x00, 0x00] }
            case ruInstructionFunctionUnsetReturn baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = defaultRuVmVariables { variableStack = [[retvar]], returnVariable = defaultRuVariable}
                    resultState `shouldBe` state { variables = vmVarr }
        it "Delete var" $ do
            let var = RuVariable { ruVariableValue = Int32 42, ruVariableType = ruVariableTypeInt, ruVariableId = 0x01, ruMutable = True }
            let vmVar = defaultRuVmVariables { variableStack = [[var]]}
            let state = baseState { variables = vmVar, workerCode = [0x00, 0x00, 0x00, 0x01] }
            case ruInstructionFunctionDeleteVar baseInfo (state) of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right resultState -> do
                    let vmVarr = defaultRuVmVariables { variableStack = [[]]}
                    resultState `shouldBe` state { variables = vmVarr }
    describe "Unset Arg" $ do
        let info = RuVmInfo { --osef
            stringTable = [ "\0" ],
            functionTable = [],
            code = [],
            codeSize = 0x00,
            dumpMode = False
        }
        let varStr0 = defaultRuVariable {
            ruVariableValue = Str "osef",
            ruVariableType = ruVariableTypeStr,
            ruVariableId = 0x00
        }
        let varStr1 = varStr0 {
            ruVariableId = 0x01
        }
        let varInt2 = defaultRuVariable {
            ruVariableValue = Int32 0xffff,
            ruVariableType = ruVariableTypeInt,
            ruVariableId = 0x02
        }
        let arg0 = defaultRuVariable {
            ruVariableValue = Str "Bretzel",
            ruVariableType = ruVariableTypeStr,
            ruVariableId = 0x00
        }
        let arg1 = defaultRuVariable {
            ruVariableValue = Str "Lemoncello",
            ruVariableType = ruVariableTypeStr,
            ruVariableId = 0x01
        }
        let arg2 = defaultRuVariable {
            ruVariableValue = Str "Prostituées",
            ruVariableType = ruVariableTypeStr,
            ruVariableId = 0x02
        }
        let variabless = defaultRuVmVariables {
            variableStack = [ [varStr0, varInt2], [varStr1] ],
            argumentVariables = [ [arg2], [arg0], [arg2] ]
        }
        let commonState = baseState {
            variables = variabless
        }
        it "Unset arg in function scope" $ do
            --                             varId               argId
            let ccode = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            let state = commonState {
                workerCode = ccode
            }
            let expectedVar = defaultRuVariable {
                ruVariableValue = ruVariableValue arg0,
                ruVariableType = ruVariableType arg0,
                ruVariableId = ruVariableId varStr0
            }
            let expected = state {
                variables = variabless {
                    variableStack = [ [expectedVar, varInt2], [varStr1] ]
                }
            }
            case ruInstructionFunctionUnsetArg info state of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right newState -> newState `shouldBe` expected
        it "Set arg in global scope" $ do
            --                          varId                   argId
            let ccode = [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
            let state = commonState {
                workerCode = ccode
            }
            let expectedVar = defaultRuVariable {
                ruVariableValue = ruVariableValue arg0,
                ruVariableType = ruVariableType arg0,
                ruVariableId = ruVariableId varStr1
            }
            let expected = state {
                variables = variabless {
                    variableStack = [ [varStr0, varInt2], [expectedVar] ]
                }
            }
            case ruInstructionFunctionUnsetArg info state of
                Left err -> do
                    putStrLn ("Error encountered: " ++ show err)
                    False `shouldBe` True --Fail
                Right newState -> newState `shouldBe` expected
        it "Doesn't set unknow arg" $ do
            --                          varId                   argId
            let ccode = [0x00, 0x00, 0x00, 0x01, 0x99, 0x00, 0x86, 0x74]
            let state = commonState {
                workerCode = ccode
            }
            ruInstructionFunctionUnsetArg info state `shouldBe` Left (ruExceptionUnknowArgument 0x99008674)
        it "Doesn't set unknow variable" $ do
            --                          varId                   argId
            let ccode = [0x65, 0x87, 0x65, 0x78, 0x00, 0x00, 0x00, 0x00]
            let state = commonState {
                workerCode = ccode
            }
            ruInstructionFunctionUnsetArg info state `shouldBe` Left (ruExceptionUnknowVariable 0x65876578)

        {--it "Doesn't set variable in other scope" $ do
            --                          varId                   argId
            let ccode = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            let state = commonState {
                workerCode = ccode
            }
            ruInstructionFunctionUnsetArg info state `shouldBe` Left (ruExceptionUnknowArgument 0x00000002)--}
        it "Doesn't set argument in other scope" $ do
            --                          varId                   argId
            let ccode = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02]
            let state = commonState {
                workerCode = ccode
            }
            ruInstructionFunctionUnsetArg info state `shouldBe` Left (ruExceptionUnknowArgument 0x00000002)
        {--it "Doesn't set argument to other type" $ do
            --                          varId                   argId
            let ccode = [0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00]
            let state = commonState {
                workerCode = ccode
            }
            ruInstructionFunctionUnsetArg info state `shouldBe` Left (ruExceptionUnsetArgumentInIncompatibleType)--}

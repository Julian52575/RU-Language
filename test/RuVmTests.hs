module RuVmTests where

import Test.Hspec
import Data.Either

import RuVariableModule
import RuExceptionModule
import RuFormatModule
import RuVmModule
import RuOperandModule
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

    {--
    data RuVmVariables = RuVmVariables {
    variableStack :: [[RuVariable]], -- first is current scope, last is global
    tmpVariable :: RuVariable,
    returnVariable :: RuVariable,
    argumentVariables :: [RuVariable],
    carry :: Bool
} deriving (Eq, Show) --}
--ruVmVariablesGetVariableInCurrentScope :: RuVmVariables -> Word8 -> Maybe RuVariable
    describe "ruVmVariablesGetVariableInCurrentScope" $ do
        it "Get existing variable" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [badVar, goodVar, badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariableInCurrentScope baseVariables 0x01) `shouldBe` Just goodVar
        it "Doesn't get variable from other scope" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [goodVar], [badVar], [badVar], [badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariableInCurrentScope baseVariables 0x01) `shouldBe` Just goodVar
        it "Doesn't get unknow variable" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            } 
            let baseVariables = RuVmVariables {
                variableStack = [ [badVar], [badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariableInCurrentScope baseVariables 0x01) `shouldBe` Nothing

--ruVmVariablesGetVariableInGlobalScope :: RuVmVariables -> Word8 -> Maybe RuVariable --TODO
    describe "ruVmVariablesGetVariableInGlobalScope" $ do
        it "get existing variable" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [badVar], [badVar, goodVar, badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariableInGlobalScope baseVariables 0x01) `shouldBe` Just goodVar
        it "doesn't get variable from other scope" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [badVar], [badVar], [badVar], [goodVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariableInGlobalScope baseVariables 0x01) `shouldBe` Just goodVar
        it "doesn't get unknow variable" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [goodVar, badVar], [badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariableInGlobalScope baseVariables 0x01) `shouldBe` Nothing

--ruVmVariablesSetVariableInCurrentScope :: RuVmVariables -> RuVariable -> Maybe RuVmVariables
    describe "ruVmVariablesSetVariableInCurrentScope" $ do
        it "Add variable to global scope" $ do
            let baseVariables = RuVmVariables {
                variableStack = [ [] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            let newVar = RuVariable {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0,
                ruMutable = True
            }
            let variablesResult = ruVmVariablesSetVariableInCurrentScope baseVariables newVar
            variableStack variablesResult `shouldBe` [ [newVar] ]
        it "Add variable to function scope" $ do
            let oldVar = RuVariable {
                ruVariableValue = Int32 84,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0,
                ruMutable = False
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [oldVar, oldVar, oldVar], [],  [] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            let newVar = RuVariable {
                ruVariableValue = Int32 42,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0xff,
                ruMutable = True
            }
            let expected = newVar {
                ruVariableId = 0x3
            }
            let variablesResult = ruVmVariablesSetVariableInCurrentScope baseVariables newVar
            variableStack variablesResult `shouldBe` [ [oldVar, oldVar, oldVar, expected], [], [] ]
        it "Duplicate id with other scope" $ do
            let var0 = RuVariable {
                ruVariableValue = Int32 42,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x00,
                ruMutable = True
            }
            let var1 = var0 {
                ruVariableId = 0x01
            }
            let var2 = var0 {
                ruVariableId = 0x02
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [var0], [var0, var1, var2], [] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            let variablesResult = ruVmVariablesSetVariableInCurrentScope baseVariables var0
            variableStack variablesResult `shouldBe` [ [var0, var1], [var0, var1, var2], [] ]
        it "Increase id with global scope" $ do
            let var0 = RuVariable {
                ruVariableValue = Int32 42,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x00,
                ruMutable = True
            }
            let var1 = var0 {
                ruVariableId = 0x01
            }
            let var2 = var0 {
                ruVariableId = 0x02
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [var1], [var1, var2], [var0] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            let variablesResult = ruVmVariablesSetVariableInCurrentScope baseVariables var0
            variableStack variablesResult `shouldBe` [ [var1, var2], [var1, var2], [var0] ]
        it "Doesn't change other fields" $ do
            let newVar = RuVariable {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [], [] ],
                tmpVariable = newVar,
                returnVariable = newVar,
                argumentVariables = [[newVar]],
                carry = True
            }
            let variablesResult = ruVmVariablesSetVariableInCurrentScope baseVariables newVar
            variableStack variablesResult `shouldBe` [ [newVar], [] ]
            tmpVariable variablesResult `shouldBe` tmpVariable baseVariables
            returnVariable variablesResult `shouldBe` returnVariable baseVariables
            argumentVariables variablesResult `shouldBe` argumentVariables baseVariables
            carry variablesResult `shouldBe` carry baseVariables


{-- ruVmVariablesSetVariableInGlobalScope :: RuVmVariables -> RuVariable -> Maybe RuVmVariables
 - Disabled, see function in VmModule
    describe "ruVmVariablesSetVariableInGlobalScope" $ do
        it "Add variable to global scope instead of function scope" $ do
            let baseVariables = RuVmVariables {
                variableStack = [ [], [] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            let newVar = RuVariable {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 1,
                ruMutable = True
            }
            case ruVmVariablesSetVariableInGlobalScope baseVariables newVar of
                Nothing -> False `shouldBe` True
                Just variablesResult -> do
                    variableStack variablesResult `shouldBe` [ [], [newVar] ]
        it "Doesn't add variable when duplicate id in global scope" $ do
            let newVar = RuVariable {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 1,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [], [newVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            ruVmVariablesSetVariableInGlobalScope baseVariables newVar `shouldBe` Nothing
        it "Add variable to global scope when duplicate id in other scope" $ do
            let newVar = RuVariable {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 1,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [newVar], [newVar], [] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            case ruVmVariablesSetVariableInGlobalScope baseVariables newVar of
                Nothing -> False `shouldBe` True
                Just variablesResult -> do
                    variableStack variablesResult `shouldBe` [ [newVar], [newVar], [newVar] ]
        it "Doesn't change other fields" $ do
            let newVar = RuVariable {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 1,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [], [] ],
                tmpVariable = newVar,
                returnVariable = newVar,
                argumentVariables = [[newVar]],
                carry = True
            }
            case ruVmVariablesSetVariableInGlobalScope baseVariables newVar of
                Nothing -> False `shouldBe` True
                Just variablesResult -> do
                    variableStack variablesResult `shouldBe` [ [], [newVar] ]
                    tmpVariable variablesResult `shouldBe` tmpVariable baseVariables
                    returnVariable variablesResult `shouldBe` returnVariable baseVariables
                    argumentVariables variablesResult `shouldBe` argumentVariables baseVariables
                    carry variablesResult `shouldBe` carry baseVariables
--}

--ruVmVariablesGetVariable :: RuVmVariables -> Word8 -> Maybe RuVariable
    describe "ruVmVariablesGetVariable" $ do
        it "Get variable in global scope" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [badVar], [badVar], [goodVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariable baseVariables 0x01) `shouldBe` Just goodVar
        it "Get variable in function scope" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            }           
            let goodVar = RuVariable {
                ruVariableValue = Str "Good day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [goodVar], [badVar], [badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariable baseVariables 0x01) `shouldBe` Just goodVar
        it "Doesn't get variable in other scope" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x01,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [], [badVar], [] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariable baseVariables 0x01) `shouldBe` Nothing
        it "Doesn't get unknow variable" $ do
            let badVar = RuVariable {
                ruVariableValue = Str "Bad day",
                ruVariableType = ruVariableTypeStr,
                ruVariableId = 0x02,
                ruMutable = True
            }
            let baseVariables = RuVmVariables {
                variableStack = [ [badVar], [badVar] ],
                tmpVariable = defaultRuVariable,
                returnVariable = defaultRuVariable,
                argumentVariables = [],
                carry = False
            }
            (ruVmVariablesGetVariable baseVariables 0x01) `shouldBe` Nothing
--ruVmStateReadWord8 :: RuVmState -> RuOperand -> Either RuException RuVariable
    describe "ruVmStateReadWord8" $ do
        it "Read constant" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0,
                workerCode = [0x12, 0x34, 0x56, 0x78],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            let expected = RuVariable {
                            ruVariableValue = Int32 0x12345678,
                            ruVariableType = ruVariableTypeInt,
                            ruVariableId = 0x00,
                            ruMutable = False
            }
            case ruVmStateReadWord8 state RuOperandConstant of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right var -> var `shouldBe` expected
        it "Read variable id" $ do
            let expected = RuVariable {
                            ruVariableValue = Str "Hello World",
                            ruVariableType = ruVariableTypeStr,
                            ruVariableId = 0x42,
                            ruMutable = False
            }
            let variabless = defaultRuVmVariables {
                variableStack = [ [expected ], [], [] ]
            }
            let state = RuVmState {
                variables = variabless,
                workerCodeOffset = 0,
                workerCode = [0x00, 0x00, 0x00, 0x42],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadWord8 state RuOperandVariableId of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right var -> var `shouldBe` expected
        it "Handles RuOperandConstant incomplete code" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0,
                workerCode = [0x00, 0x00, 0x00],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadWord8 state RuOperandConstant `shouldBe` (Left ruExceptionIncompleteInstruction)
        it "Handles RuOperandVariableId incomplete code" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0,
                workerCode = [0x00, 0x00, 0x00],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadWord8 state RuOperandVariableId `shouldBe` (Left ruExceptionIncompleteInstruction)

        it "Handles unknow variable id" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0,
                workerCode = [0x00, 0x00, 0x00, 0x42],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadWord8 state RuOperandVariableId `shouldBe` (Left (ruExceptionUnknowVariable 0x42))
        it "Handles RuOperandUnused" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0,
                workerCode = [0x00, 0x00, 0x00, 0x42],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadWord8 state RuOperandUnused `shouldBe` (Left ruExceptionInvalidCodingByte)
        it "Handles RuOperandNone" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0,
                workerCode = [0x00, 0x00, 0x00, 0x42],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadWord8 state RuOperandNone `shouldBe` (Right defaultRuVariable)
--ruVmStateReadOperand :: RuVmState -> Either RuException [RuVariable]
    describe "ruVmStateReadOperand" $ do
        it "Read 4 RuOperandConstant" $ do
            let expected1 = RuVariable {
                ruVariableValue = Int32 0x11111111,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x00,
                ruMutable = False
            }
            let expected2 = expected1 {
                ruVariableValue = Int32 0x22222222
            }
            let expected3 = expected1 {
                ruVariableValue = Int32 0x33333333
            }
            let expected4 = expected1 {
                ruVariableValue = Int32 0x44444444
            }
            let state = RuVmState {
                variables = defaultRuVmVariables,
                workerCodeOffset = 0x00,
                workerCode = [0xAA, 0x11, 0x11, 0x11, 0x11, 0x22, 0x22, 0x22, 0x22, 0x33, 0x33, 0x33, 0x33, 0x44, 0x44, 0x44, 0x44],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadOperand state of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right list -> list `shouldBe` [expected1, expected2, expected3, expected4]
        it "Read 4 RuOperandVariableId" $ do
            let expected1 = RuVariable {
                ruVariableValue = Int32 0x11111111,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x01,
                ruMutable = False
            }
            let expected2 = expected1 {
                ruVariableValue = Int32 0x22222222,
                ruVariableId = 0x02
            }
            let expected3 = expected1 {
                ruVariableValue = Int32 0x33333333,
                ruVariableId = 0x03
            }
            let expected4 = expected1 {
                ruVariableValue = Int32 0x44444444,
                ruVariableId = 0x04
            }
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [expected1, expected2, expected3], [expected4] ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0xFF, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadOperand state of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right list -> list `shouldBe` [expected1, expected2, expected3, expected4]
        it "Stops at RuOperandNone" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0x2A, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadOperand state of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right list -> list `shouldBe` []
        it "Read only 1 RuOperandVariableId" $ do
            let expected1 = RuVariable {
                ruVariableValue = Int32 0x11111111,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x01,
                ruMutable = False
            }
            let expected2 = expected1 {
                ruVariableValue = Int32 0x22222222,
                ruVariableId = 0x02
            }
            let expected3 = expected1 {
                ruVariableValue = Int32 0x33333333,
                ruVariableId = 0x03
            }
            let expected4 = expected1 {
                ruVariableValue = Int32 0x44444444,
                ruVariableId = 0x04
            }
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [expected1, expected2, expected3], [expected4] ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0xC0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadOperand state of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right list -> list `shouldBe` [expected1]
        it "Read only 2 RuOperandVariableId" $ do
            let expected1 = RuVariable {
                ruVariableValue = Int32 0x11111111,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x01,
                ruMutable = False
            }
            let expected2 = expected1 {
                ruVariableValue = Int32 0x22222222,
                ruVariableId = 0x02
            }
            let expected3 = expected1 {
                ruVariableValue = Int32 0x33333333,
                ruVariableId = 0x03
            }
            let expected4 = expected1 {
                ruVariableValue = Int32 0x44444444,
                ruVariableId = 0x04
            }
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [expected1, expected2, expected3], [expected4] ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0xF0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadOperand state of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right list -> list `shouldBe` [expected1, expected2]
        it "Read only 3 RuOperandVariableId" $ do
            let expected1 = RuVariable {
                ruVariableValue = Int32 0x11111111,
                ruVariableType = ruVariableTypeInt,
                ruVariableId = 0x01,
                ruMutable = False
            }
            let expected2 = expected1 {
                ruVariableValue = Int32 0x22222222,
                ruVariableId = 0x02
            }
            let expected3 = expected1 {
                ruVariableValue = Int32 0x33333333,
                ruVariableId = 0x03
            }
            let expected4 = expected1 {
                ruVariableValue = Int32 0x44444444,
                ruVariableId = 0x04
            }
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [expected1, expected2, expected3], [expected4] ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0xFC, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            case ruVmStateReadOperand state of
                Left err -> do
                    putStrLn (show err)
                    False `shouldBe` True
                Right list -> list `shouldBe` [expected1, expected2, expected3]
        it "Handle RuOperandUnused" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [  ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0x55, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadOperand state `shouldBe` Left ruExceptionInvalidCodingByte
        it "Handle Incomplete instruction" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [  ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0xAA, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00], 
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadOperand state `shouldBe` Left ruExceptionIncompleteInstruction
        it "Handle Unknow variable" $ do
            let state = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [] ]
                },
                workerCodeOffset = 0x00,
                workerCode = [0xC0, 0x00, 0x00, 0x00, 0x01], 
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateReadOperand state `shouldBe` Left (ruExceptionUnknowVariable 0x01)
    
--ruVmInfoGetStringFromStringTable :: RuVmInfo -> Word32 -> Maybe String
    describe "ruVmInfoGetStringFromStringTable" $ do
        let info = RuVmInfo {
                stringTable = [ "\0", "a", "b" ],
                functionTable = [],
                code = [],
                codeSize = 0x00,
                dumpMode = True
        }
        it "Get string" $ do
            ruVmInfoGetStringFromStringTable info 1 `shouldBe` Just "a"
        it "Doesn't get out of bound string" $ do
            ruVmInfoGetStringFromStringTable info 3 `shouldBe` Nothing

--ruVmVariablesUpdateVariable :: RuVmVariables -> Word32 -> RuVariableValue -> Either RuException RuVmVariables
    describe "ruVmVariablesUpdateVariable" $ do
        let var0 = defaultRuVariable {
            ruVariableValue = Int32 0x84,
            ruVariableType = ruVariableTypeInt,
            ruVariableId = 0x00,
            ruMutable = True
        }
        let var1 = defaultRuVariable {
            ruVariableId = 0x01
        }
        let var2 = defaultRuVariable {
            ruVariableId = 0x02
        }
        let newValue = Str "Hello World"
        let newVar1 = var1 {
            ruVariableValue = Str "Hello World",
            ruVariableType = ruVariableTypeStr,
            ruMutable = True
        }
        it "Update variable in global scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [], [var0, var1] ]
            }
            case ruVmVariablesUpdateVariable variabless 0x01 newValue of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right variablessResult -> do
                    variableStack variablessResult `shouldBe` [ [], [var0, newVar1] ]

        it "Doesn't update unknow var" $ do
            isLeft (ruVmVariablesUpdateVariable defaultRuVmVariables 0xFF newValue) `shouldBe` True

        it "Update variable in function scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var1], [], [var0] ]
            }
            case ruVmVariablesUpdateVariable variabless 0x01 newValue of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right variablessResult -> do
                    variableStack variablessResult `shouldBe` [ [newVar1], [], [var0] ]

        it "Doesn't update other var" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var1, var2], [], [var0] ]
            }
            case ruVmVariablesUpdateVariable variabless 0x01 newValue of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right variablessResult -> do
                    variableStack variablessResult `shouldBe` [ [newVar1, var2], [], [var0] ]
        
        it "Doesn't update other scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var1], [var1], [var1], [var1], [var0] ]
            }
            case ruVmVariablesUpdateVariable variabless 0x01 newValue of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right variablessResult -> do
                    variableStack variablessResult `shouldBe` [ [newVar1], [var1], [var1], [var1], [var0] ]

        it "Update variable in single scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var2, var1, var0] ]
            }
            case ruVmVariablesUpdateVariable variabless 0x01 newValue of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right variablessResult -> do
                    variableStack variablessResult `shouldBe` [ [var2, newVar1, var0] ]

--ruVmVariablesSetArgument :: RuVmVariables -> Word32 -> RuVariable -> RuVmVariables
    describe "ruVmVariablesSetArgument" $ do
        let var0 = defaultRuVariable {
            ruVariableValue = Int32 0xFF,
            ruVariableType = ruVariableTypeInt
        }
        let var1 = var0 {
            ruVariableId = 0x01
        }
        it "Create an argument" $ do
            let variabless = defaultRuVmVariables {
                argumentVariables = [ [] ]
            }
            let expected = variabless {
                argumentVariables = [ [var1] ]
            }
            ruVmVariablesSetArgument variabless 0x1 var1 `shouldBe` expected
        it "Updates an argument" $ do
            let upVar = var1 {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr
            }
            let variabless = defaultRuVmVariables {
                argumentVariables = [ [var1] ]
            }
            let expected = variabless {
                argumentVariables = [ [upVar] ]
            }
            ruVmVariablesSetArgument variabless 0x01 upVar `shouldBe` expected


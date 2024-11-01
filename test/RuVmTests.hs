module RuVmTests where

import Test.Hspec
import Data.Either

import RuVariableModule
import RuExceptionModule
import RuFormatModule
import RuVmModule
import RuOperandModule
{--
data RuVmState = RuVmState {
    variables :: RuVmVariables, 
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8],
    conditionalMode :: Bool,
    scopeDeep :: Int,
    toPrint :: String
} deriving (Eq, Show)

data RuVm = RuVm {
    stringTable :: [String], --the first string must be '\0'
    functionTable :: [RuFunctionTable],
    code :: [Word8],
    codeSize :: Word32,
    ruVmState :: RuVmState
} deriving (Eq, Show)

data RuFunctionTable = RuFunctionTable {
    nameIndex :: Word32,
    codeSectionOffset :: Word32,
    size :: Word32
} deriving (Eq, Show)

data RuHeader = RuHeader {
    fileSize :: Word32,
    fileVersion :: Word8,
    functionTableCount :: Word32,
    strTableOffset :: Word32,
    strTableCount :: Word32,
    codeOffset :: Word32,
    entrypointOffset :: Word32
} deriving(Eq, Show)

data RuFormat = RuFormat {
    ruHeader :: RuHeader,
    ruFunctionTable :: [Word8],
    strTab :: [Word8],
    codeSection :: [Word8]
} deriving (Eq, Show)
--}

spec :: Spec
spec = do
    describe "initRuVmState" $ do
        let createVar = [0x01, 0x00, 0xB0, 0x00, 0x00, 0x00, 0x01, 0xFF, 0XFF, 0XFF, 0XFF]
        let createVarSize = length createVar
        --
        let codeTab = createVar ++ createVar ++ createVar ++ createVar
        let fun1 = RuFunctionTable {
            nameIndex = 0xffffffff, --osef
            codeSectionOffset = fromIntegral createVarSize, --Commence au 2ème createVar
            size = 0xff00ff00 --osef
        }
        let header = RuHeader {
            fileSize = 0xff, --osef
            fileVersion = 0x01, --osef
            functionTableCount = 0xff, --osef
            strTableOffset = 0xff, --osef
            strTableCount = 0xff, --osef
            codeOffset = 0xff, --osef
            entrypointOffset = 0x00 --you're cool
        }
        let format = RuFormat {
            ruHeader = header, --
            ruFunctionTable = [], --osef
            strTab = [], --osef
            codeSection = codeTab
        }
        let info = RuVmInfo {
            stringTable = ["\0", "a\0"],
            functionTable = [fun1],
            code = codeTab,
            codeSize = (fromIntegral (length codeTab)),
            dumpMode = False
        }
        it "Basic convertion" $ do
            let expected = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [] ],
                    argumentVariables = [ [] ]
                },
                workerCode = drop (fromIntegral (entrypointOffset header)) codeTab,
                workerCodeOffset = (entrypointOffset header),
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateInit format info `shouldBe` expected
        it "Entrypoint offset match function start" $ do
            let header2 = header {
                entrypointOffset = codeSectionOffset fun1
            }
            let format2 = format {
                ruHeader = header2
            }
            let expected = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ [], [] ],
                    argumentVariables = [ [], [] ]
                },
                workerCode = drop (fromIntegral (entrypointOffset header2)) codeTab,
                workerCodeOffset = entrypointOffset header2,
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            ruVmStateInit format2 info `shouldBe` expected --scopeDeep 0 with 2 array of variables

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

--ruVmVariablesGetVariableInGlobalScope :: RuVmVariables -> Word8 -> Maybe RuVariable
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
                argumentVariables = [ ],
                variableStack = []
            }
            let expected = variabless {
                argumentVariables = [ [var1] ]
            }
            ruVmVariablesSetArgument variabless 0x1 var1 `shouldBe` expected
        it "Update an argument" $ do
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
        it "Create an argument in current scope" $ do
            let upVar = var1 {
                ruVariableValue = Str "Hello World",
                ruVariableType = ruVariableTypeStr
            }
            let variabless = defaultRuVmVariables {
                argumentVariables = [ [], [var1], [var1], [var1] ]
            }
            let expected = variabless {
                argumentVariables = [ [upVar], [var1], [var1], [var1] ]
            }
            ruVmVariablesSetArgument variabless 0x01 upVar `shouldBe` expected           

--ruVmVariablesRemoveArgument :: RuVmVariables -> Word32 -> RuVmVariables
    describe "ruVmVariablesRemoveArgument" $ do
        let arg0 = defaultRuVariable {
            ruVariableValue = Int32 0xFF,
            ruVariableType = ruVariableTypeInt,
            ruVariableId = 0x00
        }
        let arg1 = arg0 {
            ruVariableId = 0x01
        }
        let arg2 = arg0 {
            ruVariableId = 0x02
        }
        let variabless = defaultRuVmVariables {
            variableStack = [ [arg0, arg1, arg2] ],
            argumentVariables = [ [arg2], [arg1, arg0], [arg1, arg0] ]
        }
        it "Remove argument" $ do
            let expected = variabless {
                argumentVariables = [ [], [arg1, arg0], [arg1, arg0] ]
            }
            ruVmVariablesRemoveArgument variabless 0x02 `shouldBe` expected
        it "Doesn't remove argument from other scope" $ do
            ruVmVariablesRemoveArgument variabless 0x01 `shouldBe` variabless
        it "Doesn't remove argument from multiple scope" $ do
            let variabless2 = variabless {
                argumentVariables = [ [arg1, arg0], [arg1, arg0], [arg1, arg0] ]
            }
            let expected = variabless2 {
                argumentVariables = [ [arg0], [arg1, arg0], [arg1, arg0] ]
            }
            ruVmVariablesRemoveArgument variabless2 0x01 `shouldBe` expected
        it "Does nothign when unknow arg" $ do
            ruVmVariablesRemoveArgument variabless 0xff `shouldBe` variabless
--ruVmVariablesGetArgument :: RuVmVariables -> Word32 -> Either RuException RuVariable
    describe "ruVmVariablesGetArgument" $ do
        let arg0 = defaultRuVariable {
            ruVariableValue = Int32 0xFF,
            ruVariableType = ruVariableTypeInt,
            ruVariableId = 0x00
        }
        let arg1 = arg0 {
            ruVariableValue = Str "Hello",
            ruVariableType = ruVariableTypeStr,
            ruVariableId = 0x01
        }
        let arg2 = arg0 {
            ruVariableValue = Str "Dragon Ball Sparking Zero!",
            ruVariableType = ruVariableTypeStr,
            ruVariableId = 0x02
        }
        let variabless = defaultRuVmVariables {
            variableStack = [ [arg0] ],
            argumentVariables = [ [arg0], [arg2], [arg1, arg0] ]
        }
        it "Get a variable" $ do
            case ruVmVariablesGetArgument variabless 0x02 of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right var -> var `shouldBe` arg2
        it "Doesn't get a variable in another scope" $ do
            ruVmVariablesGetArgument variabless 0x00 `shouldBe` Left (ruExceptionUnknowArgument 0x00)
        it "Doesn't get unknow variable" $ do
            ruVmVariablesGetArgument variabless 0xff `shouldBe` Left (ruExceptionUnknowArgument 0xff)
        it "Errors out on single argument list" $ do
            let vars = defaultRuVmVariables {
                argumentVariables = [ [arg0] ]
            }
            ruVmVariablesGetArgument vars 0x00 `shouldBe` Left ruExceptionAccessArgumentInFirstScope
            
--ruVmVariablesGetVariableIndex :: RuVmVariables -> Word32 -> Maybe (Int, Int)
    describe "ruVmVariablesGetVariableIndex" $ do
        let var0 = defaultRuVariable {
            ruVariableValue = Int32 0x00,
            ruVariableType = ruVariableTypeInt,
            ruVariableId = 0x00,
            ruMutable = True
        }
        let var1 = var0 {
            ruVariableValue = Int32 0x01,
            ruVariableId = 0x01
        }
        let var2 = var1 {
            ruVariableValue = Int32 0x02,
            ruVariableId = 0x02
        }
        let osef = defaultRuVariable {
            ruVariableId = 0xff
        }
        let variabless = defaultRuVmVariables {
            variableStack = [ [osef, osef, osef, var1], [var2], [osef, var0, osef] ]
        }
        it "Get variable in function scope" $ do
            case ruVmVariablesGetVariableIndex variabless 0x01 of
                Nothing -> False `shouldBe` True
                Just xy -> xy `shouldBe` (0, 3)
        it "Get variable in global scope" $ do
            case ruVmVariablesGetVariableIndex variabless 0x00 of
                Nothing -> False `shouldBe` True
                Just xy -> xy `shouldBe` (2, 1)
        it "Doesn't get variable in other scope" $ do
            ruVmVariablesGetVariableIndex variabless 0x02 `shouldBe` Nothing

--ruVmVariablesRemoveVariable :: RuVmVariables -> Word32 -> RuVmVariables
    describe "ruVmVariablesRemoveVariable" $ do
        let var0 = defaultRuVariable {
            ruVariableValue = Int32 0x00,
            ruVariableType = ruVariableTypeInt,
            ruVariableId = 0x00,
            ruMutable = True
        }
        let var1 = var0 {
            ruVariableValue = Int32 0x01,
            ruVariableId = 0x01
        }
        let var2 = var1 {
            ruVariableValue = Int32 0x02,
            ruVariableId = 0x02
        }  
        it "Remove the variable in function scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var0, var1], [var2] ]
            }            
            let expected = [ [var0], [var2] ]
            variableStack (ruVmVariablesRemoveVariable variabless 0x01) `shouldBe` expected
        it "Remove the variable in global scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var0, var2], [var1] ]
            }            
            let expected = [ [var0, var2], [] ]
            variableStack (ruVmVariablesRemoveVariable variabless 0x01) `shouldBe` expected
        it "Does nothing when unknow variable" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var1, var2], [var0] ]
            }
            ruVmVariablesRemoveVariable variabless 0xff `shouldBe` variabless
        it "Doesn't remove variable from other scope" $ do
            let variabless = defaultRuVmVariables {
                variableStack = [ [var1], [var2], [var0] ]
            }
            ruVmVariablesRemoveVariable variabless 0x02 `shouldBe` variabless

--ruVmStateJump :: RuVmInfo -> RuVmState -> Word32 -> Either RuException RuVmState
    describe "ruVmStateJump" $ do
        let createVar = [0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0xFF, 0xFF, 0xFF, 0xFF]
        let returnIns = [0x02, 0x00]
        let createVarSize = fromIntegral (length createVar)
        let globalCode = createVar ++ createVar
        let globalCodeSize = fromIntegral (length globalCode)
        let funCode = createVar ++ returnIns
        let funSize = fromIntegral (length funCode)
        let codeTab = globalCode ++ funCode ++ funCode ++ funCode
        --
        let fun1 = RuFunctionTable {
            nameIndex = 1,
            codeSectionOffset = globalCodeSize,
            size = funSize
        }
        let fun2 = fun1 {
            codeSectionOffset = (codeSectionOffset fun1) + (size fun1)
        }
        let fun3 = fun2 {
            codeSectionOffset = (codeSectionOffset fun2) + (size fun2)
        }
        let info = RuVmInfo {
            stringTable = [ "\0", "fun1\0", "fun2\0", "fun3\0" ],
            functionTable = [fun1] ++ [fun2] ++ [fun3],
            code = codeTab,
            codeSize = fromIntegral (length codeTab),
            dumpMode = False
        }
        let state = RuVmState {
            variables = defaultRuVmVariables,
            workerCodeOffset = 0x00,
            workerCode = code info,
            conditionalMode = False,
            scopeDeep = 0x00,
            toPrint = []
        }
        it "Move forward in global scope" $ do
            case ruVmStateJump info state createVarSize of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right newState -> do
                    workerCodeOffset newState `shouldBe` createVarSize
                    workerCode newState `shouldBe` (createVar ++ funCode ++ funCode ++ funCode)
                    take 2 (workerCode newState) `shouldBe` [0x01, 0x00]
        it "Move forward in function scope" $ do
            let fun2State = state {
                workerCode = drop (fromIntegral (codeSectionOffset fun2)) (workerCode state),
                workerCodeOffset = codeSectionOffset fun2
            }
            workerCode fun2State `shouldBe` (funCode ++ funCode)
            case ruVmStateJump info fun2State createVarSize of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right newState -> do
                    workerCodeOffset newState `shouldBe` ((workerCodeOffset fun2State) + createVarSize)
                    workerCode newState `shouldBe` (returnIns ++ funCode)
                    take 2 (workerCode newState) `shouldBe` returnIns
        it "Move backward" $ do
            case ruVmStateJump info state createVarSize of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right movedState -> do
                    let neg = fromIntegral ( (word32ToInt32 createVarSize) * (-1))
                    case ruVmStateJump info movedState neg of
                        Left err -> do
                            putStrLn ("Encountered exception: " ++ (show err) ++ (show neg) )
                            False `shouldBe` True
                        Right newState -> newState `shouldBe` state
        it "Move backward in function scope" $ do
            let fun2State = state {
                workerCode = drop (fromIntegral (codeSectionOffset fun2)) (workerCode state),
                workerCodeOffset = codeSectionOffset fun2
            }
            workerCode fun2State `shouldBe` (funCode ++ funCode)
            case ruVmStateJump info fun2State createVarSize of
                Left err -> do
                    putStrLn ("Encountered exception: " ++ (show err))
                    False `shouldBe` True
                Right movedState -> do
                    let neg = fromIntegral ( (word32ToInt32 createVarSize) * (-1))
                    case ruVmStateJump info movedState neg of
                        Left err -> do
                            putStrLn ("Encountered exception: " ++ (show err))
                            False `shouldBe` True
                        Right newState -> newState `shouldBe` fun2State
        it "Doesn't jump to another scope" $ do
            case ruVmStateJump info state (createVarSize * 2) of
                Left err -> err `shouldBe` ruExceptionJumpOutOfScope
                Right _ -> False `shouldBe` True
        it "Doesn't jump out of bound" $ do
            case ruVmStateJump info state 2147483647 of
                Left err -> err `shouldBe` ruExceptionJumpOutOfBound
                Right _ -> False `shouldBe` True

--ruVmStateCreateNewScope :: RuVmState -> RuVmState
--ruVmStateExitScope :: RuVmState -> RuVmState
    describe "ruVmStateCreateNewScope" $ do
        let var = defaultRuVariable {
            ruVariableValue = Str "Lol",
            ruVariableType = ruVariableTypeStr
        }
        it "Create the first scope" $ do
            let baseState = RuVmState {
                variables = defaultRuVmVariables {
                    variableStack = [ ],
                    argumentVariables = [],
                    callOffsets = []
                },
                workerCodeOffset = 0x15,
                workerCode = [ 0xFF ],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            let expected = baseState {
                variables = (variables baseState) {
                    variableStack = [ [] ],
                    argumentVariables = [ [] ],
                    callOffsets = [ workerCodeOffset baseState ]
                },
                scopeDeep = 1
            }
            ruVmStateCreateScope baseState `shouldBe` expected
        it "Create the nth scope" $ do
            let vars = defaultRuVmVariables {
                variableStack = [ [var, var], [var] ],
                argumentVariables = [ [var, var], [var] ],
                callOffsets = [0x30, 0x20, 0x10]
            }
            let baseState = RuVmState {
                variables = vars,
                workerCodeOffset = 0x40,
                workerCode = [ 0xFF ],
                conditionalMode = False,
                scopeDeep = 2,
                toPrint = []
            }
            let expected = baseState {
                variables = (variables baseState) {
                    variableStack = [ [], [var, var], [var] ],
                    argumentVariables = [ [], [var, var], [var] ],
                    callOffsets = [workerCodeOffset baseState, 0x30, 0x20, 0x10]
                },
                scopeDeep = (scopeDeep baseState) + 1
            }
            ruVmStateCreateScope baseState `shouldBe` expected
    describe "ruVmStateExitScope" $ do
        let var = defaultRuVariable {
            ruVariableValue = Str "Lol",
            ruVariableType = ruVariableTypeStr
        }
        it "Remove a scope" $ do
            let vars = defaultRuVmVariables {
                variableStack = [ [var], [var] ],
                argumentVariables = [ [var], [var] ],
                callOffsets = [0xab]
            }
            let baseState = RuVmState {
                variables = vars,
                workerCodeOffset = 0x15,
                workerCode = [ 0xFF ],
                conditionalMode = False,
                scopeDeep = 1,
                toPrint = []
            }
            let expected = baseState {
                variables = (variables baseState) {
                    variableStack = [ [var] ],
                    argumentVariables = [ [var] ],
                    callOffsets = []
                },
                workerCodeOffset = 0xab,
                scopeDeep = 0
            }
            ruVmStateExitScope baseState `shouldBe` expected
        it "Empty the only scope" $ do
            let vars = defaultRuVmVariables {
                variableStack = [ [var] ],
                argumentVariables = [ [var] ],
                callOffsets = []
            }
            let baseState = RuVmState {
                variables = vars,
                workerCodeOffset = 0x15,
                workerCode = [ 0xFF ],
                conditionalMode = False,
                scopeDeep = 0,
                toPrint = []
            }
            let expected = baseState {
                variables = (variables baseState) {
                    variableStack = [ [] ],
                    argumentVariables = [ [] ],
                    callOffsets = []
                },
                scopeDeep = -1
            }
            ruVmStateExitScope baseState `shouldBe` expected

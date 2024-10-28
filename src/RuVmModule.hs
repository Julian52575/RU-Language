module RuVmModule where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Char
import Data.Either
import Data.Maybe
import Data.List(head, last, tail, init, find)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import RuOperandModule
import RuVariableModule
import RuFormatModule as RF
import RuExceptionModule


{-- RuVmVariables
 --}
data RuVmVariables = RuVmVariables {
    variableStack :: [[RuVariable]], -- first is current scope, last is global
    tmpVariable :: RuVariable,
    returnVariable :: RuVariable,
    argumentVariables :: [[RuVariable]],
    carry :: Bool
} deriving (Eq, Show)

defaultRuVmVariables :: RuVmVariables
defaultRuVmVariables = RuVmVariables {
    variableStack = [ [] ], --only global stack array is created
    tmpVariable = defaultRuVariable,
    returnVariable = defaultRuVariable,
    argumentVariables = [],
    carry = False
}


ruVmVariablesGetBiggestId :: RuVmVariables -> Word32
ruVmVariablesGetBiggestId variabless
    | length stack == 0 = 0
    | length stack == 1 = fromIntegral (length (head stack))
    | otherwise         = fromIntegral (length (head stack) + length (last stack))
    where
        stack = (variableStack variabless)


ruVmVariablesSetVariableInCurrentScope :: RuVmVariables -> RuVariable -> RuVmVariables
ruVmVariablesSetVariableInCurrentScope variabless newVar
    | length stack == 0                       = variabless { variableStack = [ [newVar] ] }      --Stack vide -> new tab
    | length stack == 1                       = variabless { variableStack = [ globalStack ++ [newVarId] ] }  --1 seul stack (globale) -> ajout variable
    | otherwise                               = variabless { variableStack = ( [(scopeStack ++ [newVarId])] ++ middleStacks ++ [globalStack] ) }
    where
        stack = (variableStack variabless)
        scopeStack = head stack
        globalStack = last stack
        middleStacks = (init. tail) stack
        newId = ruVmVariablesGetBiggestId variabless
        newVarId = newVar {
            ruVariableId = newId
        }

{-- Disabled due to variable id conflict when assigning a global variable when fun stack is already filled
ruVmVariablesSetVariableInGlobalScope :: RuVmVariables -> RuVariable -> Maybe RuVmVariables
ruVmVariablesSetVariableInGlobalScope variabless newVar
    | length stack == 0                       = Just variabless { variableStack = [ [newVar] ] } --pas de tab
    | (isNothing globalSearchResult) == False = Nothing --déjà présente
    | length stack == 1                       = Just variabless { variableStack = [ globalStack ++ [newVar] ] }
    | otherwise                               = Just variabless { variableStack = ( firstStacks ++ [(globalStack ++ [newVar])] ) } 
    where
        stack = (variableStack variabless)
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless (ruVariableId newVar)
        globalStack = last stack
        firstStacks = init stack
--}
    
ruVmVariablesGetVariableInCurrentScope :: RuVmVariables -> Word32 -> Maybe RuVariable
ruVmVariablesGetVariableInCurrentScope variabless idd
    | length stack == 0 = Nothing --Rien à trouver
    | otherwise         = searchResult --Les resultats de la recherche
    where
        stack = (variableStack variabless)
        scopeStack = head stack
        searchResult = find (\ruVarParser -> ruVariableHasId ruVarParser idd) scopeStack

ruVmVariablesGetVariableInGlobalScope :: RuVmVariables -> Word32 -> Maybe RuVariable
ruVmVariablesGetVariableInGlobalScope variabless idd
    | length stack == 0 = Nothing --Rien à trouver
    | otherwise         = searchResult --Les resultats de la recherche
    where
        stack = (variableStack variabless)
        globalStack = last stack
        searchResult = find (\ruVarParser -> ruVariableHasId ruVarParser idd) globalStack

ruVmVariablesGetVariable :: RuVmVariables -> Word32 -> Maybe RuVariable
ruVmVariablesGetVariable variabless idd
    | (isNothing globalSearchResult == False) = globalSearchResult
    | (isNothing scopeSearchResult == False)  = scopeSearchResult
    | otherwise                               = Nothing
    where
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless idd
        scopeSearchResult = ruVmVariablesGetVariableInCurrentScope variabless idd

{-- VmState
 --}
data RuVmState = RuVmState {
    variables :: RuVmVariables, 
    workerCodeOffset :: Word32, -- similar to PC
    workerCode :: [Word8],
    conditionalMode :: Bool,
    scopeDeep :: Int,
    toPrint :: String
} deriving (Eq, Show)

ruVmStateReadWord8 :: RuVmState -> RuOperand -> Either RuException RuVariable --TODO
ruVmStateReadWord8 _ RuOperandUnused = Left ruExceptionInvalidCodingByte
ruVmStateReadWord8 _ RuOperandNone = Right defaultRuVariable
ruVmStateReadWord8 state RuOperandConstant
    | length tab < 4      = Left ruExceptionIncompleteInstruction
    | otherwise           = Right RuVariable {
                                ruVariableValue = Int64 valueInt,
                                ruVariableType = ruVariableTypeInt,
                                ruVariableId = 0x00,
                                ruMutable = False
    }
    where
        tab = workerCode state
        a = tab !! 0
        b = tab !! 1
        c = tab !! 2
        d = tab !! 3
        value32bit = word84ToWord32 a b c d
        valueInt = fromIntegral value32bit
ruVmStateReadWord8 state RuOperandVariableId
    | length tab < 4      = Left ruExceptionIncompleteInstruction
    | otherwise           = case searchResult of
                                Nothing -> Left (ruExceptionUnknowVariable value32bit)
                                Just var -> Right var
    where
        tab = workerCode state
        a = tab !! 0
        b = tab !! 1
        c = tab !! 2
        d = tab !! 3
        value32bit = word84ToWord32 a b c d
        searchResult = ruVmVariablesGetVariable (variables state) value32bit

ruVmStateParseOperand :: RuVmState -> [RuOperand] -> Either RuException [RuVariable]
ruVmStateParseOperand _ []                            = Right []
ruVmStateParseOperand _ (RuOperandNone:_)             = Right []
ruVmStateParseOperand _ (RuOperandUnused:_)           = Left ruExceptionInvalidCodingByte
ruVmStateParseOperand state (currentOp:nextOp)      = do
    let opSize = (ruOperandToSize currentOp)
    let opSizeInt = fromIntegral opSize
    let nextState = state {
        workerCode = drop opSizeInt (workerCode state),
        workerCodeOffset = ((workerCodeOffset state) + opSize)
    }
    let result = ruVmStateReadWord8 state currentOp
    let nextResult = ruVmStateParseOperand nextState nextOp
    case result of
        Left err -> Left err
        Right var1 -> case nextResult of 
            Left err -> Left err
            Right var2 -> Right ([var1] ++ var2)


{-- Call this function instead of ruVmStateReadOperand
 --}
ruVmStateReadOperand :: RuVmState -> Either RuException [RuVariable]
ruVmStateReadOperand state
    | length ccode == 0                = Left ruExceptionIncompleteInstruction
    | (length ccode) - 1 < operandSize = Left ruExceptionIncompleteInstruction --Code is shorter than operandSize
    | otherwise = ruVmStateParseOperand stateWithoutCodingByte list       
    where
        ccode = workerCode state
        codingByte = ccode !! 0
        list = codingByteToRuOperand codingByte
        operandSize = fromIntegral (codingByteToOperandsSize codingByte)
        stateWithoutCodingByte = state {
            workerCode = drop 1 (workerCode state),
            workerCodeOffset = ((workerCodeOffset state) + 1)
        }


checkVmStateCodeOffset :: RuVmInfo -> RuVmState -> Maybe RuException
checkVmStateCodeOffset info state
    | workerCodeOffset state > codeSize info = Just ruExceptionInvalidProgramCounter
    | otherwise = Nothing

checkVmState :: RuVmInfo -> RuVmState -> Maybe RuException
checkVmState info state = checkVmStateCodeOffset info state



ruFormatToRuVmState :: RuFormat -> Either RuException RuVmState
ruFormatToRuVmState format = do
    let codeOffsetInt = fromIntegral (codeOffset (ruHeader format))
    Right RuVmState {
        variables = defaultRuVmVariables,
        workerCodeOffset = codeOffset (ruHeader format),
        workerCode = drop codeOffsetInt (codeSection format),
        conditionalMode = False,
        scopeDeep = 0,
        toPrint = []
    }

data RuVmInfo = RuVmInfo {
    stringTable :: [String], --the first string must be '\0'
    functionTable :: [RuFunctionTable],
    code :: [Word8],
    codeSize :: Word32,
    dumpMode :: Bool
    --vmState :: RuVmState -- Separate to ensure only VmState is updated ?
} deriving (Eq, Show)


{-- Assume RuFormat is valid
 --}
ruFormatToRuVmInfo :: RuFormat -> RuVmInfo
ruFormatToRuVmInfo format = RuVmInfo {
    stringTable = convertWord8ToStringTable (strTab format) [],
    functionTable = convertWord8ToFunctionTable (ruFunctionTable format),
    code = codeSection format,
    codeSize = codSiz,
    dumpMode = False
}
    where
        codSiz = (fileSize (ruHeader format)) - (codeOffset (ruHeader format))

{-- Get a RuVm from a fileName
 --}
fileNameToRuVm :: String -> IO (Either RuException RuVmInfo)
fileNameToRuVm fileName = runExceptT $ do --lire fichier
    byteString <- liftIO $ BS.readFile fileName
    let byteList = BS.unpack byteString
    let result = RF.fileContentToRuFormat byteList
    case result of
        Left err -> throwE err
        Right format -> do
            case ruFormatIsValid format of
                Left err -> throwE err
                Right validFormat -> return (ruFormatToRuVmInfo validFormat)



{-- Helper function to update program counter
 --}
 --
ruVmStateCheckOutOfBound :: RuVmInfo -> RuVmState -> Bool
ruVmStateCheckOutOfBound info state
    | (workerCodeOffset state) >= (codeSize info) = True
    | otherwise = False

ruVmStateUpdateWorkerCodeToPc :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruVmStateUpdateWorkerCodeToPc info state
    | ruVmStateCheckOutOfBound info state = Left ruExceptionInvalidProgramCounter
    | otherwise = Right movedState
    where
        offsetInt = fromIntegral (workerCodeOffset state)
        movedState = state {
            workerCode = drop offsetInt (code info)
        }

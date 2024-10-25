module RuVmModule where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Char
import Data.Either
import Data.Maybe
import Data.List(head, last, tail, init, find)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

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


ruVmVariablesSetVariableInCurrentScope :: RuVmVariables -> RuVariable -> Maybe RuVmVariables
ruVmVariablesSetVariableInCurrentScope variabless newVar
    | length stack == 0                       = Just variabless { variableStack = [ [newVar] ] }      --Stack vide -> new tab
    | (isNothing globalSearchResult) == False = Nothing --Variable id déjà présent dans la stack globale 
    | length stack == 1                       = Just variabless { variableStack = [ globalStack ++ [newVar] ] }  --1 seul stack (globale) -> ajout variable
    | (isNothing scopeSearchResult) == False  = Nothing --Variable id déjà présent dans la première stack (fonction)
    | otherwise                               = Just variabless { variableStack = ( [(scopeStack ++ [newVar])] ++ middleStacks ++ [globalStack] ) }
    where
        stack = (variableStack variabless)
        scopeStack = head stack
        scopeSearchResult = ruVmVariablesGetVariableInCurrentScope variabless (ruVariableId newVar)
        globalStack = last stack
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless (ruVariableId newVar)
        middleStacks = (init. tail) stack

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
    
ruVmVariablesGetVariableInCurrentScope :: RuVmVariables -> Word8 -> Maybe RuVariable
ruVmVariablesGetVariableInCurrentScope variabless idd
    | length stack == 0 = Nothing --Rien à trouver
    | otherwise         = searchResult --Les resultats de la recherche
    where
        stack = (variableStack variabless)
        scopeStack = head stack
        searchResult = find (\ruVarParser -> ruVariableHasId ruVarParser idd) scopeStack

ruVmVariablesGetVariableInGlobalScope :: RuVmVariables -> Word8 -> Maybe RuVariable
ruVmVariablesGetVariableInGlobalScope variabless idd
    | length stack == 0 = Nothing --Rien à trouver
    | otherwise         = searchResult --Les resultats de la recherche
    where
        stack = (variableStack variabless)
        globalStack = last stack
        searchResult = find (\ruVarParser -> ruVariableHasId ruVarParser idd) globalStack

ruVmVariablesGetVariable :: RuVmVariables -> Word8 -> Maybe RuVariable
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


convertWord8ToStringTable :: [Word8] -> String -> [String]
convertWord8ToStringTable (w:ws) currentStr
    | w == 0x00 = [ (currentStr ++ "\0") ] ++ convertWord8ToStringTable ws []
    | 0x01 <= w && w <= 0x7f = convertWord8ToStringTable ws (currentStr ++ [wCharTrue])
    | otherwise = convertWord8ToStringTable ws (currentStr ++ [wChar])
    where
        wChar = '.'
        wCharTrue = chr (fromIntegral w)
convertWord8ToStringTable _ _ = []


convertWord8ToFunctionTable :: [Word8] -> [RuFunctionTable]
convertWord8ToFunctionTable (n:a:m:e:o:f:f2:t:s:i:z:e2:next) = do
    [ fun ] ++ convertWord8ToFunctionTable next
    where
    fun = RuFunctionTable {
        nameIndex = word84ToWord32 n a m e,
        codeSectionOffset = word84ToWord32 o f f2 t,
        size = word84ToWord32 s i z e2
    }
convertWord8ToFunctionTable _ = []


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

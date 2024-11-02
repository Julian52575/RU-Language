module RuVmModule where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Int
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

-- https://stackoverflow.com/questions/36406553/haskell-get-the-index-of-the-first-occurrence-in-a-list-of-strings
findIndex :: (a -> b -> Bool) -> [a] -> b -> Maybe Int
findIndex p xs z=
  case [ i | (x, i) <- zip xs [0..], p x z ] of
    [] -> Nothing
    e:_ -> Just e


replaceTab :: [a] -> Int -> a -> [a]
replaceTab tab i value = take i tab ++ [value] ++ drop (i + 1) tab

removeTab :: [a] -> Int -> [a]
removeTab tab i = take i tab ++ drop (i + 1) tab

{-- RuVmVariables
 --}
data RuVmVariables = RuVmVariables {
    variableStack :: [[RuVariable]], -- first is current scope, last is global
    tmpVariable :: RuVariable,
    returnVariable :: RuVariable,
    argumentVariables :: [[RuVariable]],
    carry :: Bool,
    callOffsets :: [Word32]
} deriving (Eq, Show)

defaultRuVmVariables :: RuVmVariables
defaultRuVmVariables = RuVmVariables {
    variableStack = [ [] ], --only global stack array is created
    tmpVariable = defaultRuVariable,
    returnVariable = defaultRuVariable,
    argumentVariables = [ [] ],
    carry = False,
    callOffsets = []
}


ruVmVariablesGetBiggestId :: RuVmVariables -> Word32
ruVmVariablesGetBiggestId variabless
    | length stack == 0 = 0
    | length stack == 1 = fromIntegral (length (head stack))
    | otherwise         = fromIntegral (length (head stack) + length (last stack))
    where
        stack = (variableStack variabless)


{-- Update Var
 --}
ruVmVariablesUpdateVariable :: RuVmVariables -> Word32 -> RuVariableValue -> Either RuException RuVmVariables
ruVmVariablesUpdateVariable variabless idd value
    | ruVmVariablesGetVariable variabless idd   == Nothing   = Left (ruExceptionUnknowVariable idd)
    | otherwise = do
        let relevantArrayIndex = if globalSearchResult /= Nothing then globalScopeIndex else 0
        let relevantArray = (variableStack variabless) !! relevantArrayIndex
        case findIndex ruVariableHasId relevantArray idd of
            Nothing -> Left (ruExceptionUnknowVariable idd)
            Just relevantVarIndex -> do
                let upVar = (relevantArray !! relevantVarIndex) {
                    ruVariableValue = value,
                    ruVariableType = ruVariableValueGetVariableType value
                }
                let newArray = replaceTab relevantArray relevantVarIndex upVar
                let newStack = replaceTab (variableStack variabless) relevantArrayIndex newArray
                Right variabless {
                    variableStack = newStack
                }
    where
        globalScopeIndex = (length (variableStack variabless) - 1) 
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless idd

{-- Delete Var
 --}
ruVmVariablesDeleteVariable :: RuVmVariables -> Word32 -> RuVmVariables
ruVmVariablesDeleteVariable variabless idd
    | ruVmVariablesGetVariable variabless idd == Nothing = variabless
    | otherwise = do
        let relevantArrayIndex = if globalSearchResult /= Nothing then globalScopeIndex else 0
        let relevantArray = (variableStack variabless) !! relevantArrayIndex
        case findIndex ruVariableHasId relevantArray idd of
            Nothing -> variabless
            Just relevantVarIndex -> do
                let newArray = removeTab relevantArray relevantVarIndex
                let newStack = replaceTab (variableStack variabless) relevantArrayIndex newArray
                variabless {
                    variableStack = newStack
                }
    where
        globalScopeIndex = (length (variableStack variabless) - 1) 
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless idd

{-- Get Variable
 --}

{-- Variable Setter
 --}
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

{-- Helper function to get Variable
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
    | idd == 0xffffffff                       = Just (tmpVariable variabless)
    | (isNothing globalSearchResult == False) = globalSearchResult
    | (isNothing scopeSearchResult == False)  = scopeSearchResult
    | otherwise                               = Nothing
    where
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless idd
        scopeSearchResult = ruVmVariablesGetVariableInCurrentScope variabless idd

ruVmVariablesGetVariableIndex :: RuVmVariables -> Word32 -> Maybe (Int, Int)
ruVmVariablesGetVariableIndex variabless idd
    | scopeSearchResult  /= Nothing = Just (0, (fromMaybe fromMaybeError findIndexInScope))
    | globalSearchResult /= Nothing = Just (globalScopeIndex, (fromMaybe fromMaybeError findIndexInGlobal))
    | otherwise = Nothing
    where
        fromMaybeError = error "fromMaybe error"
        globalScopeIndex = (length (variableStack variabless) - 1)
        globalSearchResult = ruVmVariablesGetVariableInGlobalScope variabless idd
        findIndexInGlobal = findIndex ruVariableHasId ((variableStack variabless) !! globalScopeIndex) idd
        scopeSearchResult = ruVmVariablesGetVariableInCurrentScope variabless idd
        findIndexInScope = findIndex ruVariableHasId ((variableStack variabless) !! 0) idd

{-- Remove Variable
 --}
ruVmVariablesRemoveVariable :: RuVmVariables -> Word32 -> RuVmVariables
ruVmVariablesRemoveVariable variabless idd =
    case ruVmVariablesGetVariableIndex variabless idd of
        Nothing -> variabless
        Just (y, x) -> do
            let relevantArray = stack !! y
            let upArray = removeTab relevantArray x
            variabless {
                variableStack = replaceTab stack y upArray
            }
    where
        stack = variableStack variabless

{-- Helper functions for arguments
 --}
ruVmVariablesSetArgument :: RuVmVariables -> Word32 -> RuVariable -> RuVmVariables
ruVmVariablesSetArgument variabless numero var
    | length stack == 0 = variabless { argumentVariables = [ [upVar] ] }
    | otherwise = case iindex of
        Nothing -> variabless {
                    argumentVariables = ([[upVar]] ++ otherStack)
                } --variable doesn't exist
        Just index2 -> do
            let newCurrentStack = replaceTab currentStack index2 upVar
            variabless {
                argumentVariables = ([newCurrentStack] ++ otherStack)
            }
    where
        upVar = var {
            ruVariableId = numero
        }
        stack = argumentVariables variabless
        currentStack = head stack
        otherStack = tail stack
        iindex = findIndex ruVariableHasId currentStack numero


ruVmVariablesRemoveArgument :: RuVmVariables -> Word32 -> RuVmVariables
ruVmVariablesRemoveArgument variabless numero
    | length (variableStack variabless) == 0 = variabless
    | otherwise = case findIndex ruVariableHasId currentStack numero of
        Nothing -> variabless
        Just index -> do
            let newStack = removeTab currentStack index
            variabless {
                argumentVariables = replaceTab (argumentVariables variabless) index newStack
            }
    where
        currentStack = (argumentVariables variabless) !! 0


ruVmVariablesGetArgument :: RuVmVariables -> Word32 -> Either RuException RuVariable
ruVmVariablesGetArgument vars numero
    | length args < 2    = Left ruExceptionAccessArgumentInFirstScope
    | otherwise         =
        case searchResult of
            Nothing -> Left (ruExceptionUnknowArgument numero)
            Just var -> Right var
    where
        args = argumentVariables vars
        arg = args !! 1
        searchResult = find (\ruVarParser -> ruVariableHasId ruVarParser numero) arg

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

{-- Read operand
 --}
ruVmStateReadWord8 :: RuVmState -> RuOperand -> Either RuException RuVariable
ruVmStateReadWord8 _ RuOperandUnused = Left ruExceptionInvalidCodingByte
ruVmStateReadWord8 _ RuOperandNone = Right defaultRuVariable
ruVmStateReadWord8 state RuOperandConstant
    | length tab < 4      = Left ruExceptionIncompleteInstruction
    | otherwise           = Right RuVariable {
                                ruVariableValue = Int32 value32bit,
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

{-- Helper function for code displacement
 --}

word32ToInt32 :: Word32 -> Int32
word32ToInt32 w = fromIntegral w
--  | w > 0x7FFFFFFF = fromIntegral w - 0x100000000
--  | otherwise      = fromIntegral w

int32ToWord32 :: Int32 -> Word32
int32ToWord32 i = fromIntegral i

    -- Comment on gère les négatifs avec word32 ? On converti en Int32
ruVmStateJump :: RuVmInfo -> RuVmState -> Word32 -> Either RuException RuVmState
ruVmStateJump info state offset
    | fromIntegral newOffset > length (code info)    = Left ruExceptionJumpOutOfBound --En dehors du code
    | currentFunSearchResult == Nothing = do
        if funSearchResult == Nothing
            then Right updatedState -- c'est OK
            else Left ruExceptionJumpOutOfScope --Pas la même scope
    | funSearchResult == Nothing        = Left ruExceptionJumpOutOfBound
    | otherwise                         = Right updatedState
    where
        newOffsetInt = (word32ToInt32 (workerCodeOffset state)) + (word32ToInt32 offset)
        newOffset = int32ToWord32 newOffsetInt
        tab = functionTable info
        funSearchResult = ruFunctionTableGetFunctionFromCodeOffset tab newOffset
        currentFunSearchResult = ruFunctionTableGetFunctionFromCodeOffset tab (workerCodeOffset state)
        updatedState = state {
            workerCodeOffset = newOffset,
            workerCode = drop (fromIntegral newOffset) (code info)
        }


{-- Scope management
 --}
ruVmStateCreateScope :: RuVmState -> RuVmState
ruVmStateCreateScope state = newState
    where
        oldVars = variables state
        newVariables = oldVars {
            variableStack = ([[]] ++ (variableStack oldVars)),
            argumentVariables = ([[]] ++ (argumentVariables oldVars)),
            callOffsets = [workerCodeOffset state] ++ callOffsets oldVars
        }
        newState = state {
            variables = newVariables,
            scopeDeep = (scopeDeep state) + 1
        }

ruVmStateExitScope :: RuVmState -> RuVmState
ruVmStateExitScope state = do
    let newStack = if length oldStack > 1 then drop 1 oldStack else [[]]
    let newArg = if length oldArg > 1 then drop 1 oldArg else [[]]
    state {
        variables = oldVars {
            variableStack = newStack,
            argumentVariables = newArg,
            callOffsets = drop 1 (callOffsets oldVars)
        },
        scopeDeep = (scopeDeep state) - 1,
        workerCodeOffset = if length (callOffsets oldVars) > 0 then (callOffsets oldVars) !! 0 else workerCodeOffset state
    }
    where
        oldVars = variables state
        oldStack = variableStack oldVars
        oldArg = argumentVariables oldVars


{-- VmState error handling
 --}
checkVmStateCodeOffset :: RuVmInfo -> RuVmState -> Maybe RuException
checkVmStateCodeOffset info state
    | workerCodeOffset state > codeSize info = Just ruExceptionInvalidProgramCounter
    | otherwise = Nothing

checkVmState :: RuVmInfo -> RuVmState -> Maybe RuException
checkVmState info state = checkVmStateCodeOffset info state

isEntrypointOffsetStartOfFunction :: [RuFunctionTable] -> Word32 -> Bool
isEntrypointOffsetStartOfFunction [] _ = False
isEntrypointOffsetStartOfFunction (current:next) offset
    | codeSectionOffset current == offset = True
    | otherwise                           = isEntrypointOffsetStartOfFunction next offset

ruVmStateInit :: RuFormat -> RuVmInfo -> RuVmState
ruVmStateInit format info = do
    let isStartOfFunction = isEntrypointOffsetStartOfFunction (functionTable info) (entrypointOffset (ruHeader format))
    let startingVariablesArray = case isStartOfFunction of
                        True -> [ [], [] ]
                        False -> [ [] ]
    RuVmState {
        variables = defaultRuVmVariables {
            variableStack = startingVariablesArray,
            argumentVariables = startingVariablesArray
        },
        workerCodeOffset = entrypointOffset (ruHeader format),
        workerCode = drop (fromIntegral (entrypointOffset (ruHeader format))) (codeSection format),
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


removeNonAsciiFromString :: String -> String
removeNonAsciiFromString (c:cs)
    | (ord c) < 32 = removeNonAsciiFromString cs
    | otherwise             = [c] ++ removeNonAsciiFromString cs
removeNonAsciiFromString [] = []

ruVmInfoGetStringFromStringTable :: RuVmInfo -> Word32 -> Maybe String
ruVmInfoGetStringFromStringTable info i
    | iInt >= length tab = Nothing
    | otherwise       = Just ( removeNonAsciiFromString (tab !! iInt) )
    where
        iInt = fromIntegral i
        tab = stringTable info

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

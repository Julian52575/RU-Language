module RuInstructionsModule where

import Data.Word (Word8, Word32)
import Data.Maybe

import RuExceptionModule
import RuVmModule
import RuVariableModule
import RuFormatModule
import RuOperandModule

data RuInstruction = RuInstruction {
    ruInstructionPrefix :: Word8,
    ruInstructionInfix :: Word8,
    ruInstructionName :: String,
    ruInstructionFunction :: (RuVmInfo -> RuVmState -> Either RuException RuVmState),
    fixedSize :: Word32 --Taille de l'instruction si pas coding byte
}

{-- NOOP
 --}
ruInstructionNoop :: RuInstruction
ruInstructionNoop = RuInstruction {
    ruInstructionPrefix = 0x00,
    ruInstructionInfix = 0x00,
    ruInstructionName = "NOOP",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 2
}
ruInstructionFunctionNoop :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionNoop _ state = Right state

{-- print
 --}
ruInstructionPrint :: RuInstruction
ruInstructionPrint = RuInstruction {
    ruInstructionPrefix = 0x00,
    ruInstructionInfix = 0x01,
    ruInstructionName = "PRINT",
    ruInstructionFunction = ruInstructionFunctionPrint,
    fixedSize = 2
}

ruInstructionFunctionPrint :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionPrint _ state = do
    let ccode = workerCode state
    --let codeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 3 then Left ruExceptionIncompleteInstruction
    else do
        let operand = ccode !! 2
        let newState = state { toPrint = show operand }
        Right newState

{-- PrintLn
 --}
ruInstructionPrintLn :: RuInstruction
ruInstructionPrintLn = RuInstruction {
    ruInstructionPrefix = 0x00,
    ruInstructionInfix = 0x02,
    ruInstructionName = "PRINTLN",
    ruInstructionFunction = ruInstructionFunctionPrintLn,
    fixedSize = 2
}

ruInstructionFunctionPrintLn :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionPrintLn _ state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 3 then Left ruExceptionIncompleteInstruction
    else do
        let operand = ccode !! 2
        let newState = state { toPrint = show operand ++ "\n" }
        Right newState

{-- Create var
--}
ruInstructionCreateVar :: RuInstruction
ruInstructionCreateVar = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x00,
    ruInstructionName = "CREATEVAR",
    ruInstructionFunction = ruInstructionFunctionCreateVar,
    fixedSize = 10
}

getWord32FromOperand :: [Word8] -> Word32
getWord32FromOperand operand = do
    let var = word8ArrayToWord32 operand
    if var == Nothing then 0
    else fromJust var

ruInstructionGetVariableFromCode :: [Word8] -> RuVmInfo -> RuVariable
ruInstructionGetVariableFromCode ccode info = do
    let operand1 = take 4 ccode
    let operand2 = take 4 (drop 4 ccode)
    let var = defaultRuVariable { ruVariableType = (operand1 !! 3) }
    if operand1 !! 3 == ruVariableTypeInt then var { ruVariableValue = Int32 (getWord32FromOperand operand2) }
    else if operand1 !! 3 == ruVariableTypeStr then var { ruVariableValue = Str (case ruVmInfoGetStringFromStringTable info (getWord32FromOperand operand2) of
        Nothing -> ""
        Just str -> str) }
    else var { ruVariableValue = Na }

ruInstructionFunctionCreateVar :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionCreateVar vminfo state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 8 then Left ruExceptionIncompleteInstruction
    else do
        let var = ruInstructionGetVariableFromCode ccode vminfo
        let newVariables = ruVmVariablesSetVariableInCurrentScope (variables state) var
        let newState = state { variables = newVariables }
        Right newState

ruInstructionGetRuVariableFromBytes :: [Word8] -> [Word8] -> RuOperand -> RuVmInfo -> RuVmState -> Maybe RuVariable
ruInstructionGetRuVariableFromBytes operand1 operand2 codingOperand info state
    | operand1 !! 3 == ruVariableTypeInt && codingOperand == RuOperandConstant = Just defaultRuVariable { ruVariableType = 0x01, ruVariableValue = Int32 (getWord32FromOperand operand2) }
    | operand1 !! 3 == ruVariableTypeStr && codingOperand == RuOperandConstant = Just defaultRuVariable { ruVariableType = 0x02, ruVariableValue = Str (case ruVmInfoGetStringFromStringTable info (getWord32FromOperand operand2) of
        Nothing -> ""
        Just str -> str) }
    -- | RuOperand !! 1 == 0b11 = var
    | otherwise = var
        where
            var = ruVmVariablesGetVariableInCurrentScope (variables state) (getWord32FromOperand operand2)

ruInstructionSetTmpVar :: RuInstruction
ruInstructionSetTmpVar = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x02,
    ruInstructionName = "SETTMPVAR",
    ruInstructionFunction = ruInstructionFunctionSetTmpVar,
    fixedSize = 0
}

ruInstructionFunctionSetTmpVar :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionSetTmpVar vminfo state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 9 then Left ruExceptionIncompleteInstruction
    else do
        let codingByte = take 1 ccode
        let codingOperand = codingByteToRuOperand (codingByte !! 0)
        let operand1 = take 4 (drop 1 ccode)
        let operand2 = take 4 (drop 5 ccode)
        let var = ruInstructionGetRuVariableFromBytes operand1 operand2 (codingOperand !! 1) vminfo state
        if var == Nothing then Left $ ruExceptionIncompleteInstruction
        else do
            let newVariables = (variables state) { tmpVariable = fromJust var }
            let newState = state { variables = newVariables }
            Right newState

ruInstructionSetVar :: RuInstruction
ruInstructionSetVar = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x01,
    ruInstructionName = "SETVAR",
    ruInstructionFunction = ruInstructionFunctionSetVar,
    fixedSize = 0
}

ruInstructionGetValueFromBytes :: [Word8] -> RuOperand -> RuVmState -> RuVariableValue
ruInstructionGetValueFromBytes operand codingOperand state
    | codingOperand == RuOperandConstant = Int32 (getWord32FromOperand operand)
    | codingOperand == RuOperandVariableId = case ruVmVariablesGetVariableInCurrentScope (variables state) (getWord32FromOperand operand) of
        Just var -> ruVariableValue var
        Nothing -> Na
    | otherwise = Na

ruInstructionFunctionSetVar :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionSetVar _ state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 9 then Left ruExceptionIncompleteInstruction
    else do
        let codingByte = take 1 ccode
        let codingOperand = codingByteToRuOperand (codingByte !! 0)
        let operand1 = take 4 (drop 1 ccode)
        let operand2 = take 4 (drop 5 ccode)
        let value = ruInstructionGetValueFromBytes operand2 (codingOperand !! 1) state

        let var = ruVmVariablesUpdateVariable (variables state) (getWord32FromOperand operand1) value
        case var of
            Left err -> Left err
            Right newVariables -> do
                let newState = state { variables = newVariables }
                Right newState

ruInstructionSetArg :: RuInstruction
ruInstructionSetArg = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x03,
    ruInstructionName = "SETARG",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionUnsetArg :: RuInstruction
ruInstructionUnsetArg = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x04,
    ruInstructionName = "UNSETARG",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 8
}

ruInstructionSetReturn :: RuInstruction
ruInstructionSetReturn = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x05,
    ruInstructionName = "SETRETURN",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionUnsetReturn :: RuInstruction
ruInstructionUnsetReturn = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x06,
    ruInstructionName = "UNSETRETURN",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 6
}

ruInstructionDeleteVar :: RuInstruction
ruInstructionDeleteVar = RuInstruction {
    ruInstructionPrefix = 0x01,
    ruInstructionInfix = 0x07,
    ruInstructionName = "DELETEVAR",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 6
}



ruInstructionReturn :: RuInstruction
ruInstructionReturn = RuInstruction {
    ruInstructionPrefix = 0x02,
    ruInstructionInfix = 0x00,
    ruInstructionName = "RETURN",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 2
}

ruInstructionCall :: RuInstruction
ruInstructionCall = RuInstruction {
    ruInstructionPrefix = 0x02,
    ruInstructionInfix = 0x01,
    ruInstructionName = "CALL",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 6
}

ruInstructionJump :: RuInstruction
ruInstructionJump = RuInstruction {
    ruInstructionPrefix = 0x02,
    ruInstructionInfix = 0x02,
    ruInstructionName = "JUMP",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 6
}

ruInstructionJumpCarry :: RuInstruction
ruInstructionJumpCarry = RuInstruction {
    ruInstructionPrefix = 0x02,
    ruInstructionInfix = 0x03,
    ruInstructionName = "JUMPCARRY",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 6
}

ruInstructionJumpNotCarry :: RuInstruction
ruInstructionJumpNotCarry = RuInstruction {
    ruInstructionPrefix = 0x02,
    ruInstructionInfix = 0x04,
    ruInstructionName = "JUMPNOTCARRY",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 6
}

ruInstructionDoOperation :: String -> RuVariable -> RuVariable -> Maybe RuVariableValue
ruInstructionDoOperation "ADD" var1 var2 = case (ruVariableValue var1, ruVariableValue var2) of
    (Int32 value1, Int32 value2) -> Just (Int32 (value1 + value2))
    (Str value1, Str value2) -> Just (Str (value1 ++ value2))
    _ -> Nothing
ruInstructionDoOperation "SUB" var1 var2 = case (ruVariableValue var1, ruVariableValue var2) of
    (Int32 value1, Int32 value2) -> Just (Int32 (value1 - value2))
    _ -> Nothing


ruInstructionOperator :: String -> RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionOperator operation info state = do
    let ccode = workerCode state
    let ccodeOffset = workerCodeOffset state
    let ccodeSize = length ccode
    if ccodeSize < 17 then Left ruExceptionIncompleteInstruction
    else do
        let codingByte = take 1 ccode
        let codingOperand = codingByteToRuOperand (codingByte !! 0)
        let operand1 = take 4 (drop 1 ccode)
        let operand2 = take 4 (drop 5 ccode)
        let operand3 = take 4 (drop 9 ccode)
        let operand4 = take 4 (drop 13 ccode)
        let var1 = ruInstructionGetRuVariableFromBytes operand1 operand2 (codingOperand !! 1) info state
        let var2 = ruInstructionGetRuVariableFromBytes operand3 operand4 (codingOperand !! 3) info state
        if var1 == Nothing || var2 == Nothing then Left ruExceptionInvalidOperation
        else do
            let result = ruInstructionDoOperation operation (fromJust var1) (fromJust var2)
            if result == Nothing || (fromJust result) == Na then Left ruExceptionInvalidOperation
            else do
                let var = defaultRuVariable { ruVariableType = (ruVariableType (fromJust var1)), ruVariableValue = fromJust result }
                let variabless = variables state
                let newVariables = variabless { returnVariable = var }
                let newState = state { variables = newVariables }
                Right newState

ruInstructionAdd :: RuInstruction
ruInstructionAdd = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x00,
    ruInstructionName = "ADD",
    ruInstructionFunction = ruInstructionFunctionAdd,
    fixedSize = 0
}

ruInstructionFunctionAdd :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionAdd info state = ruInstructionOperator "ADD" info state

ruInstructionSub :: RuInstruction
ruInstructionSub = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x01,
    ruInstructionName = "SUB",
    ruInstructionFunction = ruInstructionFunctionSub,
    fixedSize = 0
}

ruInstructionFunctionSub :: RuVmInfo -> RuVmState -> Either RuException RuVmState
ruInstructionFunctionSub info state = ruInstructionOperator "SUB" info state

ruInstructionDiv :: RuInstruction
ruInstructionDiv = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x02,
    ruInstructionName = "DIV",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionMul :: RuInstruction
ruInstructionMul = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x03,
    ruInstructionName = "MUL",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionMod :: RuInstruction
ruInstructionMod = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x06,
    ruInstructionName = "MOD",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionEq :: RuInstruction
ruInstructionEq = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x04,
    ruInstructionName = "EQ?",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionNeq :: RuInstruction
ruInstructionNeq = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x05,
    ruInstructionName = "NEQ?",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionLesser :: RuInstruction
ruInstructionLesser = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x07,
    ruInstructionName = "LESSER?",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionLesserEq :: RuInstruction
ruInstructionLesserEq = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x08,
    ruInstructionName = "LESSEREQ?",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionGreater :: RuInstruction
ruInstructionGreater = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x09,
    ruInstructionName = "GREATER?",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}

ruInstructionGreaterEq :: RuInstruction
ruInstructionGreaterEq = RuInstruction {
    ruInstructionPrefix = 0x03,
    ruInstructionInfix = 0x0a,
    ruInstructionName = "GREATEREQ?",
    ruInstructionFunction = ruInstructionFunctionNoop,
    fixedSize = 0
}
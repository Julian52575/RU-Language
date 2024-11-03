module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Exception
import Data.Word
import Data.Either
import Data.Maybe
import Text.Printf (printf)
import Numeric

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import RuFormatModule
import RuVmModule
import RuOperandModule
import RuVariableModule
import RuExceptionModule
import RuInstructionsHelperModule
import RuInstructionsModule

{--
crousAsciiArt :: String
crousAsciiArt = "                             .################:                             \n" ++
    "                        ############################                         \n" ++
    "                    ####################################                     \n" ++
    "                 ##########################################                  \n" ++
    "              ################################################               \n" ++
    "            ####################################################             \n" ++
    "          ########################################################           \n" ++
    "         ##########################################################          \n" ++
    "       ##############################################################        \n" ++
    "      ################################################################       \n" ++
    "     ##################################################################      \n" ++
    "    ####################################################################     \n" ++
    "   ######################################################################    \n" ++
    "  ########################################################################   \n" ++
    "  ########################################################################   \n" ++
    " ##########################################################################  \n" ++
    " ##########################################################################  \n" ++
    "#####################################:.##################################### \n" ++
    "##################################        ################################## \n" ++
    "#################################  *#####==###   :##*    .###  +##.  ##      \n" ++
    "#################################  ##########   ###   ##:  ##  +##.  ##  ### \n" ++
    "#################################  ##########  ####  ####  ##  +##.  ###    _\n" ++
    "##################################   ##   ###  ####.  ##   ###  :.  ### ###  \n" ++ 
    "####################################    #####  ######-   ######   :#####   . \n" ++
    " ##########################################################################  \n" ++ 
    " ##########################################################################  \n" ++
    "  #########################################################################   \n" ++
    "  ########################################################################   \n" ++
    "   ######################################################################    \n" ++
    "    ####################################################################     \n" ++
    "     ##################################################################      \n" ++
    "      ################################################################       \n" ++
    "       ##############################################################        \n" ++
    "         ##########################################################          \n" ++
    "          ########################################################           \n" ++
    "            ####################################################             \n" ++
    "              ################################################               \n" ++
    "                 ##########################################                  \n" ++
    "                    ####################################                     \n" ++
    "                        ############################                         \n" ++
    "                             +################+                              "
--}
theColorRed :: String
theColorRed = "\ESC[31m"

theColorDefault :: String
theColorDefault = "\ESC[0m"

theColorBlue :: String
theColorBlue = "\ESC[34m"

theColorYellow :: String
theColorYellow = "\ESC[33m"

setNewWorkerCodePc :: RuVmInfo -> RuVmState -> Word32 -> Either RuException RuVmState
setNewWorkerCodePc info state newPc
    | newPc > codeSize info     = Left ruExceptionJumpOutOfBound
    | otherwise                 = Right state {
        workerCodeOffset = newPc,
        workerCode = drop (fromIntegral newPc) (code info)
    }

--from Coding Byte
moveWorkerCodeToNextInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
moveWorkerCodeToNextInstruction ins info state
    | fixedSize ins /= 0            = setNewWorkerCodePc info state (currentPc + (fixedSize ins))
    | length (workerCode state) < 3 = Left ruExceptionIncompleteInstruction
    | otherwise          = setNewWorkerCodePc info state (currentPc + 2 + 1 + (operandsSize))
    where
        codingByte = (workerCode state) !! 2
        operandsSize = codingByteToOperandsSize codingByte
        currentPc = workerCodeOffset state

-- Ajoute le nom de l'instruction dans le buffer de print
myShowHex :: Word8 -> String --showHex is GARBAGE
myShowHex w = printf "%02x" w 
--
getInstructionCodeAsString :: [ Word8 ] -> Word32 -> Word32 -> Word32 -> String
getInstructionCodeAsString (current:next) start count limit
    | start >= limit = theColorDefault ++ "\n"
    | otherwise = do
        let color = if count < 2 then theColorBlue else if count == 2 then theColorDefault else []
        color ++ myShowHex current ++ " " ++ getInstructionCodeAsString next (start + 1) (count + 1) limit

getInstructionCodeAsString [] _ _ _ = []
--
getFunctionNameToPrint :: RuVmInfo -> RuVmState -> Maybe String
getFunctionNameToPrint info state = case funSearchResult of
    Nothing -> Nothing
    Just fun -> case ruVmInfoGetStringFromStringTable info (nameIndex fun) of
        Nothing -> Just "??? (name index does not match any starting offset of function table):"
        Just name -> Just name
    where
        funSearchResult = ruFunctionTableGetFunctionFromStartOffset (functionTable info) (workerCodeOffset state)
--
printInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
printInstruction ins info state =
    case movedResult of
        Left err -> Left err
        Right movedState -> do
            let newPc = workerCodeOffset movedState
            let functionPrint = case getFunctionNameToPrint info state of 
                    Nothing -> []
                    Just funName -> "\n" ++ pcStr ++ " <" ++ theColorYellow ++ funName ++ theColorDefault ++ ">:\n"
            let codePrint = getInstructionCodeAsString (workerCode state) startPc 0 newPc
            Right (movedState {
                toPrint = (toPrint state) ++ functionPrint ++ name ++ codePrint
            })
    where
        startPc = workerCodeOffset state
        pcStr = printf "0x%08x" startPc
        name = pcStr ++ ":\t" ++ ruInstructionName ins ++ ":\t"
        movedResult = moveWorkerCodeToNextInstruction ins info state

-- Execute la fonction liée à l'instruction

-- Execute la fonction liée à l'instruction
execInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
execInstruction ins info state
    | length (workerCode state) < 2 = Left ruExceptionIncompleteInstruction
    | otherwise =
        case function info movedState of --résultat de function
            Left err -> Left err
            Right newState ->
                if workerCodeOffset newState == ogPc
                then--si le pc est le même, on déplace à l'instruction suivante
                    case nextInsResult of
                        Left err -> Left err
                        Right nextInsState -> Right newState {
                                        workerCode = (workerCode nextInsState),
                                        workerCodeOffset = (workerCodeOffset nextInsState)
                        }
                else
                    Right newState
    where
        ogPc = workerCodeOffset state
        movedState = state {
            workerCode = drop 2 (workerCode state),
            workerCodeOffset = ogPc --offset should stay on ins
        }
        nextInsResult = moveWorkerCodeToNextInstruction ins info state
        function = ruInstructionFunction ins
            --Removed calculation for nextPc to leave work to subFUnction
        --operandSize = codingByteToOperandsSize (workerCode state !! 0)
        --nextPc = ogPc + (if fixedSize ins == 0 then 1 + operandSize else (fixedSize ins) )
                                    --coding byte + taille de l'operand --taille fix sans le mnemonic

-- Recupère l'instruction et appelle les sous fonctions
runInstruction :: RuVmInfo -> RuVmState -> Either RuException RuVmState
runInstruction info state =
    case instructionSearch of --Check recherche d'instruction
        Nothing -> Left (ruExceptionUnknowInstruction prefix iinfix)
        Just ins -> 
            case (dumpMode info) of --Check dumpMode
                True -> printInstruction ins info state
                False -> execInstruction ins info state
    where
        codeTab = (workerCode state)
        prefix = codeTab !! 0
        iinfix = codeTab !! 1
        instructionSearch = getRuInstruction prefix iinfix

{-- Debug Print
 --}
printCodeDebug :: Int -> [Word8] -> Int -> Int -> IO () --00 00 00 00 00 00 00 00 (8x) \t 8x \n
printCodeDebug (0) _ _ _ = putStrLn []
printCodeDebug _ [] _ _ = putStrLn "."
printCodeDebug limit tab 8 1 = do
    putStrLn []
    printCodeDebug limit tab 0 0
printCodeDebug limit tab 8 0 = do
    putStr "\t"
    printCodeDebug limit tab 0 1
printCodeDebug limit (current:next) count group = do
    printf "%02x" current
    putStr " "
    printCodeDebug (limit - 1) next (count + 1) group

printCodeBeforePc :: RuVmInfo -> Word32 -> IO ()
printCodeBeforePc info pc = do
    if pc > 0x20
        then do
            printf "0x%08x:\t" (startIndex2)
            printCodeDebug len (take 16 (drop ((fromIntegral startIndex2)) (code info))) 0 0
        else putStr []
    printf "0x%08x:\t" (startIndex1)
    printCodeDebug len (take 16 (drop (fromIntegral startIndex1) (code info))) 0 0
    where
        startIndex2 = if pc < 0x20 then 0x00 else pc - 0x20
        startIndex1 = if pc < 0x10 then 0x00 else pc - 0x10
        len        = if pc < 0x10 then fromIntegral pc else 16

printCodeDebugMain :: RuVmInfo -> RuVmState -> IO ()
printCodeDebugMain info state = do
    let pc = workerCodeOffset state
    printCodeBeforePc info (pc)
    printf "0x%08x:\t" pc
    printCodeDebug 16 (take 16 (workerCode state)) 0 0
    printf "0x%08x:\t" (pc + 0x10)
    printCodeDebug 16 (take 16 (drop 16 (workerCode state))) 0 0

printVariableArrayDebug :: [ RuVariable ] -> IO ()
printVariableArrayDebug (current:next) = do
    putStr ("ID " ++ show (ruVariableId current) ++ ":\t")
    printRuVariable current
    printVariableArrayDebug next
printVariableArrayDebug [] = return ()

printVariableStack :: [ [RuVariable] ] -> IO ()
printVariableStack stack
    | stackNumber == 0          = putStrLn "🏜️ No variable in stack."
    | stackNumber == 1          = do
        let len = length (stack !! 0)
        putStrLn ("🥞 Stack variables (" ++ show len ++ "):")
        if len == 0 then putStr "Empty." else printVariableArrayDebug (stack !! 0)
    | otherwise                 = do
        let global = (stack !! (stackNumber - 1))
        let len = length global
        putStrLn ("🌎 Global variables (" ++ show len ++ "):")
        if length global == 0 then putStrLn "Empty." else printVariableArrayDebug (stack !! 0)
        printVariableStack [stack !! 0]
    where
        stackNumber = length stack

printCallOffsetsDebug :: [Word32] -> IO ()
printCallOffsetsDebug (current:next) = do
    putStrLn (printf "0x%08x" current)
    printCallOffsetsDebug next
printCallOffsetsDebug [] = return ()

printVariablesDebug :: RuVmVariables -> IO ()
printVariablesDebug vars = do
    printVariableStack (variableStack vars)
    putStrLn "📥 Argument variables:"
    if length (argumentVariables vars) < 2
    then 
        putStr "Empty."
    else
        printVariableArrayDebug ((argumentVariables vars) !! 1)
    putStrLn "📤 Return variable:"
    if returnVariable vars == defaultRuVariable
    then
        putStrLn "NA"
    else
        printRuVariable (returnVariable vars)

    if length (callOffsets vars) == 0
    then
        putStr []
    else
        putStrLn ("📍 Call offset (" ++ (show (length (callOffsets vars))) ++ "):") >>
        printCallOffsetsDebug (callOffsets vars)
    putStrLn "\n"

printStateDebug :: RuVmInfo -> RuVmState -> IO ()
printStateDebug info state = do
    if dumpMode info == False
    then do
        printVariablesDebug (variables state)
    else putStr []
    putStrLn "🤓 Code:"
    printCodeDebugMain info state

{-- Exception handelr
 --}
handleException :: RuVmInfo -> RuVmState -> RuException -> IO ()
handleException info state err = do
    let pc = workerCodeOffset state
    putStrLn (("\n😭 Encountered error at offset " ++ printf "0x%08x" pc) ++ ": " ++ theColorRed ++ show err ++ theColorDefault)

    -- Print function name if inside function
    case ruFunctionTableGetFunctionFromCodeOffset (functionTable info) pc of
        Nothing -> putStr []
        Just fun -> case ruVmInfoGetStringFromStringTable info (nameIndex fun) of
            Nothing -> putStr []
            Just funName -> putStrLn ("👉 In function " ++ funName ++ ":")

    putStrLn "👁  Here are some debug information:"
    putStrLn "\n"
    printStateDebug info state
    putStrLn "\n😔 Aborted execution..."
    exitWith(ExitFailure 84)

--
retVarToExitCode :: RuVariableValue -> ExitCode
retVarToExitCode (Int32 i)
    | i == 0    = ExitSuccess
    | otherwise = ExitFailure (fromIntegral i)
retVarToExitCode _ = ExitSuccess

exitRuVm :: RuVmInfo -> RuVmState -> IO ()
exitRuVm info state =
    case dumpMode info of
        True -> return ()
        False -> case ruVariableType retVar of
            0x01 -> do
                let value = retVarToExitCode (ruVariableValue retVar)
                exitWith(value)
            0x02 -> printRuVariable (retVar) -- On a finit le travail
            _ -> return ()
    where
        retVar = returnVariable (variables state)

{-- Main loop
 --}
ruVmLoop :: RuVmInfo -> RuVmState -> IO ()
ruVmLoop info state
    | scopeDeep state <= (-1)       = exitRuVm info state
    | workerCode state == [] =
        case dumpMode info of
            True -> exitRuVm info state
            False -> handleException info state (RuException "Reach end of code without return or call.")
    | toPrint state /= []           = do -- Fait les print et recursif
        putStr (toPrint state)
        ruVmLoop info (state { toPrint = [] })
    | length (workerCode state) < 2 = handleException info state ruExceptionIncompleteInstruction
    | otherwise                     =
        case runInstruction info state of
            Left err -> handleException info state err
            Right newState -> ruVmLoop info newState

{-- --}
data Argument = Argument {
    dump :: Bool,
    fileName :: Maybe String,
    usagePrint :: Bool,
    unknowFlag :: Maybe String
} deriving (Eq, Show)

defaultArgument :: Argument
defaultArgument = Argument {
    dump = False,
    fileName = Nothing,
    usagePrint = False,
    unknowFlag = Nothing
}

usage :: String
usage = "Usage:\t./ruvm [--dump] filename"

parseArgument :: [String] -> Argument -> Argument
--parseArgument ("--dump":next) arg = parseArgument next (arg {dump = True})

--parseArgument ("--help":_)      arg = arg {usagePrint = True}
--parseArgument (file:next) arg = parseArgument next (arg {fileName = Just file})
--parseArgument _ arg = arg
--parseArgument ("--dump":next)   arg = parseArgument next (arg {dump = True})
parseArgument (input:next) arg = 
    case input of
        ('-':flag) ->
            case flag of
                "h"    -> arg {usagePrint = True}
                "-help" -> arg {usagePrint = True}
                "-dump" -> parseArgument next (arg {dump = True})
                _      -> arg {unknowFlag = (Just ("-" ++ flag))}
        _ ->  parseArgument next (arg {fileName = Just input})
parseArgument _ arg = arg

printRuFormatIfArg :: RuFormat -> Argument -> IO ()
printRuFormatIfArg format arg
    | dump arg == True = do
        putStr theColorRed
        --putStrLn crousAsciiArt
        putStr theColorDefault
        printRuFormat format
    | otherwise        = return () 

argumentToRuVmInfo :: Argument -> IO (Either RuException (RuVmInfo, RuVmState))
argumentToRuVmInfo arg = runExceptT $ do
    case fileName arg of
        Nothing -> throwE (RuException "No file provided.")
        Just file -> do
            result <- liftIO $ fileNameToRuFormat file
            case result of
                Left err -> throwE err
                Right format -> do
                    let info = ruFormatToRuVmInfo format
                    let state = ruVmStateInit format info
                    liftIO $ printRuFormatIfArg format arg
                    return (info {
                        dumpMode = (dump arg)
                    }, state)

checkArgument :: Argument -> IO ()
checkArgument arg
    | usagePrint arg == True    = do
        putStrLn usage
        exitWith(ExitSuccess)
    | isNothing (unknowFlag arg) == False = do
        putStrLn ("🚨 Error: unknow flag:\t" ++ fromJust (unknowFlag arg))
        putStrLn "Aborting..."
        exitWith(ExitFailure 84)
    | otherwise = return ()

runMain :: IO ()
runMain = do
    args <- getArgs
    let arg = parseArgument args defaultArgument
    checkArgument arg
    result <- argumentToRuVmInfo arg
    case result of
        Left err -> putStrLn ("Encountered exception: " ++ show err)
        Right (info, state) -> ruVmLoop info state


handler :: IOException -> IO ()
handler e = do
    putStrLn ("💀 An error occured: " ++ show (e :: IOException))
    putStrLn "Aborting..."
    exitWith(ExitFailure 84)


main :: IO ()
main = catch runMain handler

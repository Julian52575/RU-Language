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

theColorRed :: String
theColorRed = "\ESC[31m"

theColorDefault :: String
theColorDefault = "\ESC[0m"

theColorBlue :: String
theColorBlue = "\ESC[34m"


data Argument = Argument {
    dump :: Bool,
    fileName :: Maybe String
} deriving (Eq, Show)

defaultArgument = Argument {
    dump = False,
    fileName = Nothing
}

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
--
printInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
printInstruction ins info state =
    case movedResult of
        Left err -> Left err
        Right movedState -> do
            let newPc = workerCodeOffset movedState
            let codePrint = getInstructionCodeAsString (workerCode state) startPc 0 newPc
            Right (movedState {
                toPrint = (toPrint state) ++ name ++ codePrint
            })
    where
        pcStr = printf "0x%08x" (workerCodeOffset state)
        name = pcStr ++ ":\t" ++ ruInstructionName ins ++ ":\t"
        movedResult = moveWorkerCodeToNextInstruction ins info state
        startPc = workerCodeOffset state

-- Execute la fonction liée à l'instruction
execInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
execInstruction ins info state
    | length (workerCode movedState) < 2 = Left ruExceptionIncompleteInstruction
    | otherwise =
        case function info movedState of --résultat de function
            Left err -> Left err
            Right newState ->
                case workerCodeOffset newState of --si le pc est le même, on déplace à l'instruction suivante
                    ogPc -> 
                        case movedResult of
                            Left err -> Left err
                            Right movedState -> Right newState {
                                            workerCode = (workerCode movedState),
                                            workerCodeOffset = (workerCodeOffset movedState)
                            }
                    _ -> Right newState
    where
        movedState = state {
            workerCode = drop 2 (workerCode state),
            workerCodeOffset = (workerCodeOffset state) + 2
        }
        movedResult = moveWorkerCodeToNextInstruction ins info state
        ogPc = workerCodeOffset movedState
        function = ruInstructionFunction ins
        operandSize = codingByteToOperandsSize (workerCode state !! 0)
        nextPc = ogPc + (if fixedSize ins == 0 then 1 + operandSize else (fixedSize ins) )
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
printCodeDebug _ _ _ _ = return ()

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
        putStrLn []
    | otherwise                 = do
        let global = (stack !! (stackNumber - 1))
        let len = length global
        putStrLn ("🌎 Global variables (" ++ show len ++ "):")
        if length global == 0 then putStr "Empty." else printVariableArrayDebug (stack !! 0)
        putStrLn []
        printVariableStack [stack !! 0]
    where
        stackNumber = length stack

printVariablesDebug :: RuVmVariables -> IO ()
printVariablesDebug vars = do
    printVariableStack (variableStack vars)
    putStrLn "📥 Argument variables:"
    if length (argumentVariables vars) < 2 then putStr "Empty." else printVariableArrayDebug ((argumentVariables vars) !! 1)
    putStrLn "\n"
    putStrLn "📤 Return variable:"
    if returnVariable vars == defaultRuVariable then putStr "NA" else printRuVariable (returnVariable vars)
    putStrLn "\n"

printStateDebug :: RuVmInfo -> RuVmState -> IO ()
printStateDebug info state = do
    if dumpMode info == False
    then do
        let vars = variables state
        printVariablesDebug (variables state)
    else putStr []
    putStrLn "🤓 Code:"
    printCodeDebugMain info state

{-- Exception handelr
 --}
handleException :: RuVmInfo -> RuVmState -> RuException -> IO ()
handleException info state except = do
    let pc = workerCodeOffset state
    putStrLn (("\n😭 Encountered error at offset " ++ printf "0x%08x" pc) ++ ": " ++ theColorRed ++ show except ++ theColorDefault)
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
            0x02 -> putStrLn [] >> printRuVariable (retVar) -- On a finit le travail
            _ -> return ()
    where
        retVar = returnVariable (variables state)

{-- Main loop
 --}
ruVmLoop :: RuVmInfo -> RuVmState -> IO ()
ruVmLoop info state
    | workerCode state == [] =
        case dumpMode info of
            True -> exitRuVm info state
            False -> handleException info state (RuException "Reach end of code without return or call.")
    | toPrint state /= []           = do -- Fait les print et recursif
        putStr (toPrint state)
        ruVmLoop info (state { toPrint = [] })
    | scopeDeep state == (-1)       = exitRuVm info state
    | length (workerCode state) < 2 = handleException info state ruExceptionIncompleteInstruction
    | otherwise                     =
        case runInstruction info state of
            Left err -> handleException info state err
            Right newState -> ruVmLoop info newState
    where
        codeSave = workerCode state
        pcSave = workerCodeOffset state

{-- --}
usage :: String
usage = "Usage:\t./ru_vm [--dump] filename"

parseArgument :: [String] -> Argument -> Argument
parseArgument ("--dump":next) arg = parseArgument next (arg {dump = True})
parseArgument (file:next) arg = parseArgument next (arg {fileName = Just file})
parseArgument _ arg = arg

printRuFormatIfArg :: RuFormat -> Argument -> IO ()
printRuFormatIfArg format arg
    | dump arg == True = do
        putStr theColorRed
        putStrLn crousAsciiArt
        putStr theColorDefault
        printRuFormat format
    | otherwise        = return () 

argumentToRuVmInfo :: [String] -> IO (Either RuException (RuVmInfo, RuVmState))
argumentToRuVmInfo str = runExceptT $ do
    let arg = parseArgument str defaultArgument
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

runMain :: IO ()
runMain = do
    args <- getArgs
    result <- argumentToRuVmInfo args
    case result of
        Left err -> putStrLn ("Encountered exception: " ++ show err)
        Right (info, state) -> ruVmLoop info state


handler :: IOException -> IO ()
handler e = do
    putStrLn ("Caught Exception: " ++ show (e :: IOException))
    exitWith(ExitFailure 84)


main :: IO ()
main = catch runMain handler

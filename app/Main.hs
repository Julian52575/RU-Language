module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Exception
import Data.Word
import Data.Either
import Data.Maybe
import Text.Printf (printf)

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import RuFormatModule
import RuVmModule
import RuOperandModule
import RuVariableModule
import RuExceptionModule
import RuInstructionsHelperModule
import RuInstructionsModule

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
    | otherwise          = setNewWorkerCodePc info state (currentPc + operandsSize)
    where
        codingByte = (workerCode state) !! 2
        operandsSize = codingByteToOperandsSize codingByte
        currentPc = workerCodeOffset state

-- Ajoute le nom de l'instruction dans le buffer de print
printInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
printInstruction ins info state =
    case moveWorkerCodeToNextInstruction ins info state of
        Left err -> Left err
        Right state -> Right (state {
            toPrint = newPrint
        })
    where
        msg = "\t" ++ ruInstructionName ins ++ "\n"
        newPrint = (toPrint state) ++ msg

-- Execute la fonction liée à l'instruction
execInstruction :: RuInstruction -> RuVmInfo -> RuVmState -> Either RuException RuVmState
execInstruction ins info state
    | length (workerCode state) < 2 = Left ruExceptionIncompleteInstruction
    | isLeft movedResult            = movedResult
    | otherwise =
        case function info movedState of --résultat de function
            Left err -> Left err
            Right newState ->
                case workerCodeOffset newState of --si le pc est le même, on déplace à l'instruction suivante
                    ogPc -> setNewWorkerCodePc info state newPc
                    _ -> Right newState
    where
        ogPc = workerCodeOffset state
        movedResult = setNewWorkerCodePc info state 2
        movedState = fromRight (error "fromRight error") movedResult
        function = ruInstructionFunction ins
        operandSize = codingByteToOperandsSize ((workerCode movedState) !! 0)
        newPc = ogPc + if fixedSize ins == 0 then operandSize + 2 else fixedSize ins

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
        movedState = state {
            workerCode = drop 2 (workerCode state)
        }
        instructionSearch = getRuInstruction prefix iinfix

{-- Debug Print
 --}
printCodeDebug :: Int -> [Word8] -> Int -> Int -> IO () --00 00 00 00 00 00 00 00 (8x) \t 8x \n
printCodeDebug (-1) _ _ _ = putStrLn []
printCodeDebug _ [] _ _ = putStrLn "End of code."
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
    printf "0x%08x:\t" (startIndex)
    printCodeDebug len (take 16 (drop (fromIntegral startIndex) (code info))) 0 0
    where
        startIndex = if pc < 0x10 then 0x00 else pc - 0x10
        len        = if pc < 0x10 then fromIntegral pc else 16

printCodeDebugMain :: RuVmInfo -> RuVmState -> IO ()
printCodeDebugMain info state = do
    let pc = workerCodeOffset state
    printCodeBeforePc info (pc - 1)
    printf "0x%08x:\t" pc
    printCodeDebug 16 (take 16 (workerCode state)) 0 0
    printf "0x%08x:\t" (pc + 0x10)
    printCodeDebug 16 (take 16 (drop 16 (workerCode state))) 0 0

printVariableArrayDebug :: [ RuVariable ] -> IO ()
printVariableArrayDebug (current:next) = do
    printRuVariable current >> printVariableArrayDebug next
printVariableArrayDebug [] = return ()

printVariableStack :: [ [RuVariable] ] -> IO ()
printVariableStack stack
    | stackNumber == 0          = putStrLn "🏜️ No variable in stack."
    | stackNumber == 1          = do
        putStrLn "🥞 Stack variables:\t"
        if length (stack !! 0) == 0 then putStr "Empty." else printVariableArrayDebug (stack !! 0)
        putStrLn "\n"
    | otherwise                 = do
        let global = (stack !! (stackNumber - 1))
        putStrLn "🌎 Global variables:\t"
        if length global == 0 then putStr "Empty." else printVariableArrayDebug (stack !! 0)
        putStrLn "\n"
        printVariableStack [stack !! 0]
    where
        stackNumber = length stack

printVariablesDebug :: RuVmVariables -> IO ()
printVariablesDebug vars = do
    printVariableStack (variableStack vars)
    putStrLn "📥 Argument variables:\t"
    if length (argumentVariables vars) < 2 then putStr "Empty." else printVariableArrayDebug ((argumentVariables vars) !! 1)
    putStrLn "\n"

printStateDebug :: RuVmInfo -> RuVmState -> IO ()
printStateDebug info state = do
    let vars = variables state
    printVariablesDebug (variables state)
    putStrLn "Code:"
    printCodeDebugMain info state

{-- Exception handelr
 --}
handleException :: RuVmInfo -> RuVmState -> RuException -> IO ()
handleException info state except = do
    let pc = workerCodeOffset state
    putStrLn (("\n😭 Encountered error at offset " ++ printf "0x%08x" pc) ++ ": " ++ show except)
    printStateDebug info state
    putStrLn "\n😔 Aborted execution..."

exitRuVm :: RuVmInfo -> RuVmState -> IO ()
exitRuVm info state = 
    case dumpMode info of
        True -> return ()
        False -> printRuVariable (returnVariable (variables state)) -- On a finit le travail

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
    | dump arg == True = printRuFormat format
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

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Exception
import Data.Word
import Data.Either
import Data.Maybe

import RuFormatModule
import RuVmModule
import RuOperandModule
import RuVariableModule
import RuExceptionModule
import RuInstructionsHelperModule


{-- Move Pc to the new instruction if previous instruction didn't
getNextPcState :: Word8 -> Word8 -> RuVmState -> RuVmState -> RuVmState
getNextPcState pre inf oldState newState
    | (oldPc /= newPc)  = newState
    | otherwise         = newState { workerCodeOffset = (newPc + (previousInstructionSize - 2)) }
    where
        oldPc = workerCodeOffset oldState
        newPc = workerCodeOffset newState
        previousInstructionSize = getInstructionSize pre inf oldState

{-- Execute chaque instruction et met à jour le PC / workerCode pour chaque 
 --}
runInstructions :: RuVmInfo -> RuVmState -> Either RuException RuVmState
runInstructions info state
    | (isLeft updatedPcResult == True)     = updatedPcResult
    | (length (workerCode movedState) < 2) = Left ruExceptionIncompleteInstruction
    | (isNothing maybeFunction == True)    = Left (ruExceptionUnknowOpcode prefix iinfix)
    | (isLeft functionResult == True)      = functionResult
    | otherwise = runInstructions info nextState
    where
    updatedPcResult = ruVmStateUpdateWorkerCodeToPc info state
    updatedPcState = (fromRight (error "fromRight error") updatedPcResult)
    prefix = (workerCode updatedPcState !! 0)
    iinfix = (workerCode updatedPcState !! 1)
    movedState = updatedPcState {
        workerCode = drop 2 (workerCode updatedPcState)
    }
    maybeFunction = getInstructionFunction prefix iinfix
    function = fromMaybe (error "Unknow Instruction") maybeFunction
    instructionOperandSize = getInstructionSize prefix iinfix
    functionResult = function info movedState
    functionResultState = fromRight (error "fromRight error") functionResult
    nextState = functionResultState
 --}

{-- Appelle runInstructions et gère le retour
 --}
runRuVm :: RuVmInfo -> Either RuException RuVariable
runRuVm info = Left (RuException "ToDo") --TODO


{-- objdump for ru
 --}

{-- --}
usage :: String
usage = "Usage:\t./ru_vm [--dump] filename"

runMain :: IO ()
runMain = do
    args <- getArgs
    case args of
        ["--usage"] -> do
            putStrLn usage
        ["--dump", filename] -> do
            result <- fileNameToRuFormat filename 
            case result of
                Left (RuException exception) -> putStrLn exception
                Right format -> printRuFormat format
        [filename] -> do
            putStrLn filename --TODO
        _ -> do
            putStrLn "No arguments provided."



handler :: IOException -> IO ()
handler e = do
    putStrLn ("Caught Exception: " ++ show (e :: IOException))
    exitWith(ExitFailure 84)


main :: IO ()
main = catch runMain handler

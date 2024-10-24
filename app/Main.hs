module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Exception

import RuException
import RuVmModule
import RuVariableModule
import RuExceptionModule



runInstructions :: RuVmInfo -> RuVmState -> Either RuException RuVmState
runInstructions state = do --Left (RuException "ToDo")
    if length (workerCode state) < 2
    then Left (ruExceptionIncompleteOpcode (workerCodeOffset state))
    else do
        let insPrefix = (workerCode state) !! 0
        let insInfix = (workerCode state) !! 1
        let movedState = state {
            workerCode = drop 2 (workerCode state)
        }
        let newState = runSingleInstruction state
        

runRuVm :: RuVmInfo -> Either RuException RuVariable
runRuVm info = Left (RuException "ToDo")
    
    
{-- --}

usage :: String
usage = "Usage:\t./ru_vm [--dump] filename"

runMain :: IO ()
runMain = do
    let vmInfo = Nothing
    args <- getArgs
    case args of
        ["--usage"] -> do
            putStrLn usage
        ["--dump"] -> do
            putStrLn "You requested a dump"
        [filename] -> do
            vmInfo = Just (fileNameToRuVm filename)
        [] -> do
            putStrLn "No arguments provided."
    if vmInfo == Nothing
    then
        putStrLn "No file to execute."
        exitWith(ExitFailure 84)
    else do
        let defaultState = ruVmTo
        let result = runRuVm (fromMaybe vmInfo)
        if isLeft result == True
        then
            putStrLn (fromLeft (RuException "RuException" result)
            exitWith(ExitFailure 84)
        else
            printRuVariable (fromRight (RuVariable {}) result)
            exitWith(ExitSuccess 0)


handler :: IOException -> IO ()
handler e = do
    putStrLn ("Caught Exception: " ++ show (e :: IOException))
    exitWith(ExitFailure 84)


main :: IO ()
main = catch runMain handler

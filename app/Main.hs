module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Exception

import RuVmModule
import RuExceptionModule


usage :: String
usage = "Usage:\t./ru_vm [--dump] filename"

runMain :: IO ()
runMain = do
    args <- getArgs
    case args of
        ["--usage"] -> do
            putStrLn usage
        ["--dump"] -> do
            putStrLn "You requested a dump"
        [filename] -> do
            putStrLn filename
        _ -> do
            putStrLn "No arguments provided."


handler :: IOException -> IO ()
handler e = do
    putStrLn ("Caught Exception" ++ show (e :: IOException))
    exitWith(ExitFailure 84)


main :: IO ()
main = catch runMain handler

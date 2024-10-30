module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Control.Exception
import Data.Word
import Data.Either
import Data.Maybe

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import RuFormatModule
import RuVmModule
import RuOperandModule
import RuVariableModule
import RuExceptionModule
import RuInstructionsHelperModule

data Argument = Argument {
    dump :: Bool,
    fileName :: Maybe String
} deriving (Eq, Show)

defaultArgument = Argument {
    dump = False,
    fileName = Nothing
}

ruVmLoop :: RuVmInfo -> RuVariable
ruVmLoop info = defaultRuVariable

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

argumentToRuVmInfo :: [String] -> IO (Either RuException RuVmInfo)
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
                    liftIO $ printRuFormatIfArg format arg
                    return info {
                        dumpMode = (dump arg)
                    }

runMain :: IO ()
runMain = do
    args <- getArgs
    result <- argumentToRuVmInfo args
    case result of
        Left err -> putStrLn ("Encountered exception: " ++ show err)
        Right info -> printRuVariable (ruVmLoop info)


handler :: IOException -> IO ()
handler e = do
    putStrLn ("Caught Exception: " ++ show (e :: IOException))
    exitWith(ExitFailure 84)


main :: IO ()
main = catch runMain handler

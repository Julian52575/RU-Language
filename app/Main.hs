module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, IOException)
import System.IO (stderr, hPutStrLn)
import Parser (parseSExprs)
import AST (sexprToAST, Ast(..))
import Evaluator (evalAST, initEnv, evalDefine, Env)
import Text.Megaparsec (parse, errorBundlePretty)
import Prelude
import Control.Monad (foldM)

import System.IO (hFlush, stdout)  -- Import hFlush and stdout

-- REPL function with delayed prompt and environment update
replLoop :: Env -> IO ()
replLoop env = do
    putStr "> "
    hFlush stdout  -- Flush the output to ensure prompt is shown immediately
    input <- getLine
    if input == "quit"
        then putStrLn "Exiting REPL..."
        else do
            let parsedExpr = parse parseSExprs "" input
            case parsedExpr of
                Left err -> do
                    hPutStrLn stderr $ "Parsing error: " ++ errorBundlePretty err
                    replLoop env  -- Continue with the same environment
                Right exprs -> do
                    let asts = mapM sexprToAST exprs
                    case asts of
                        Left err -> do
                            hPutStrLn stderr $ "Error converting to AST: " ++ err
                            replLoop env  -- Continue with the same environment
                        Right astTrees -> do
                            -- Evaluate each AST and update the environment accordingly
                            newEnv <- foldM evalInRepl env astTrees
                            replLoop newEnv  -- Continue REPL with updated environment

-- Function for evaluating a single expression in the REPL, with environment update
evalInRepl :: Env -> Ast -> IO Env
evalInRepl env expr = case expr of
    Define var defExpr -> do
        case evalDefine (Define var defExpr) env of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ err
                return env
            Right newEnv -> return newEnv
    _ -> do
        case evalAST env expr of
            Left err -> do
                hPutStrLn stderr $ "Error evaluating expression: " ++ err
                return env
            Right result -> do
                printResult result
                return env

-- Print the result of an evaluation
printResult :: Ast -> IO ()
printResult (AstInt n) = print n
printResult (AstBool True) = putStrLn "#t"
printResult (AstBool False) = putStrLn "#f"
printResult (Lambda _ _) = putStrLn "#<procedure>"
printResult _ = putStrLn "Unsupported expression"

-- Evaluate multiple ASTs with error handling
evalMultiple :: Bool -> [Ast] -> Env -> IO ()
evalMultiple _ [] _ = return ()
evalMultiple isRepl (expr:rest) env = do
    case expr of
        Define var defExpr -> do
            case evalDefine (Define var defExpr) env of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ err
                    if not isRepl then exitWith (ExitFailure 84) else return ()
                Right newEnv -> evalMultiple isRepl rest newEnv
        _ -> do
            case evalAST env expr of
                Left err -> do
                    hPutStrLn stderr $ "Error evaluating expression: " ++ err
                    if not isRepl then exitWith (ExitFailure 84) else return ()
                Right result -> printResult result
            evalMultiple isRepl rest env

usage :: String
usage = "Usage: <program> [--repl | filename]"

-- Main logic for handling arguments and executing the interpreter
runMain :: IO ()
runMain = do
    args <- getArgs
    case args of
        [] -> do
            input <- getContents
            let parsedExprs = parse parseSExprs "" input
            case parsedExprs of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitWith (ExitFailure 84)
                Right exprs -> do
                    let asts = mapM sexprToAST exprs  -- Evaluate expressions to AST
                    case asts of
                        Left err -> hPutStrLn stderr ("Error converting to AST: " ++ err) >> exitWith (ExitFailure 84)
                        Right astTrees -> evalMultiple False astTrees initEnv -- False indicates non-REPL mode
        ["--repl"] -> do
            putStrLn "Enter an expression (or 'quit' to exit):"  -- Print once at the start
            replLoop initEnv  -- Start REPL with the initial environment
        ["--help"] -> hPutStrLn stdout usage
        ["-h"] -> hPutStrLn stdout usage
        [filename] -> do
            putStrLn $ "Reading file: " ++ filename
            input <- readFile filename
            let parsedExprs = parse parseSExprs "" input
            case parsedExprs of
                Left err -> hPutStrLn stderr (errorBundlePretty err) >> exitWith (ExitFailure 84)
                Right exprs -> do
                    let asts = mapM sexprToAST exprs  -- Evaluate expressions to AST
                    case asts of
                        Left err -> hPutStrLn stderr ("Error converting to AST: " ++ err) >> exitWith (ExitFailure 84)
                        Right astTrees -> evalMultiple False astTrees initEnv -- False indicates non-REPL mode
        _ -> hPutStrLn stderr usage

-- Error handler to catch IO exceptions and exit with code 84
handler :: IOException -> IO ()
handler _ = hPutStrLn stderr "Error: IO Exception occurred." >> exitWith (ExitFailure 84)

-- Main function with error handling
main :: IO ()
main = catch runMain handler

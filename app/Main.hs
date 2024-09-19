module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, IOException)
import Parser (parseSExprs)
import AST (sexprToAST, Ast(..))
import Evaluator (evalAST, initEnv, evalDefine, Env)
import Text.Megaparsec (parse, errorBundlePretty)
import Prelude

-- REPL function with integrated parsing and evaluation
repl :: Env -> IO ()
repl env = do
    putStrLn "Enter an expression (or 'quit' to exit):"
    input <- getLine
    if input == "quit"
        then putStrLn "Exiting REPL..."
        else do
            let parsedExpr = parse parseSExprs "" input
            case parsedExpr of
                Left err -> putStrLn $ "Parsing error: " ++ errorBundlePretty err
                Right expr -> do
                    let asts = mapM sexprToAST expr
                    case asts of
                        Left err -> putStrLn $ "Error converting to AST: " ++ err
                        Right astTrees -> mapM_ (evalAndPrint True env) astTrees -- True indicates REPL mode
            repl env

-- Modified evalAndPrint function to handle errors in REPL and non-REPL mode
evalAndPrint :: Bool -> Env -> Ast -> IO ()
evalAndPrint isRepl env expr = 
    case evalAST env expr of
        Left err -> do
            putStrLn $ "Error: " ++ err
            if not isRepl then exitWith (ExitFailure 84) else return () -- Exit with 84 if not in REPL mode
        Right result -> printResult result

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
                    putStrLn $ "Error: " ++ err
                    if not isRepl then exitWith (ExitFailure 84) else return ()
                Right newEnv -> evalMultiple isRepl rest newEnv
        _ -> do
            case evalAST env expr of
                Left err -> do
                    putStrLn $ "Error evaluating expression: " ++ err
                    if not isRepl then exitWith (ExitFailure 84) else return ()
                Right result -> printResult result
            evalMultiple isRepl rest env

-- Main logic for handling arguments and executing the interpreter
runMain :: IO ()
runMain = do
    args <- getArgs
    case args of
        [] -> do
            input <- getContents
            let parsedExprs = parse parseSExprs "" input
            case parsedExprs of
                Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
                Right exprs -> do
                    let asts = mapM sexprToAST exprs  -- Evaluate expressions to AST
                    case asts of
                        Left err -> putStrLn ("Error converting to AST: " ++ err) >> exitWith (ExitFailure 84)
                        Right astTrees -> evalMultiple False astTrees initEnv -- False indicates non-REPL mode
        ["--repl"] -> repl initEnv
        [filename] -> do
            putStrLn $ "Reading file: " ++ filename
            input <- readFile filename
            let parsedExprs = parse parseSExprs "" input
            case parsedExprs of
                Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
                Right exprs -> do
                    let asts = mapM sexprToAST exprs  -- Evaluate expressions to AST
                    case asts of
                        Left err -> putStrLn ("Error converting to AST: " ++ err) >> exitWith (ExitFailure 84)
                        Right astTrees -> evalMultiple False astTrees initEnv -- False indicates non-REPL mode
        _ -> putStrLn "Usage: <program> [--repl | filename]"

-- Error handler to catch IO exceptions and exit with code 84
handler :: IOException -> IO ()
handler _ = putStrLn "Error: IO Exception occurred." >> exitWith (ExitFailure 84)

-- Main function with error handling
main :: IO ()
main = catch runMain handler

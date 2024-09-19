module Main (main) where

import System.Environment (getArgs)
import Parser (parseSExprs)
import AST (sexprToAST, Ast(..))
import Evaluator (evalAST, initEnv, evalDefine, Env)
import Text.Megaparsec (parse, errorBundlePretty)
import Prelude


repl :: IO ()
repl = do
    putStrLn "Enter a command (sum, average, ... or 'quit' to exit):"
    input <- getLine
    if input == "quit"
       then putStrLn "Exiting REPL..."
       else do
           putStrLn input
           repl


printResult :: Ast -> IO ()
printResult (AstInt n) = print n        -- Print the integer value
printResult (AstBool True) = putStrLn "#t"   -- Common Lisp true
printResult (AstBool False) = putStrLn "NIL"  -- Common Lisp false
printResult (Lambda _ _) = putStrLn "#<procedure>" -- Print a lambda function
printResult _ = putStrLn "Unsupported expression"  -- Handle unsupported cases


-- Optimized evalMultiple function
evalMultiple :: [Ast] -> Env -> IO ()
evalMultiple [] _ = return ()  -- If no more expressions, return
evalMultiple (expr:rest) env = do
    case expr of
        Define var defExpr -> do  -- If it's a definition, store it in the environment
            let newEnv = evalDefine (Define var defExpr) env
            evalMultiple rest newEnv  -- Continue with the updated environment
        _ -> do
            -- Otherwise, evaluate the expression
            case evalAST env expr of
                Nothing -> putStrLn "Error evaluating expression"
                Just result -> printResult result  -- Only print the result of expressions
            evalMultiple rest env  -- Continue with the same environment


main :: IO ()
main = do
    args <- getArgs
    case args of

        [] -> do
            input <- getContents
            let parsedExprs = parse parseSExprs "" input
            case parsedExprs of
                Left err -> putStrLn $ errorBundlePretty err
                Right exprs -> do
                    let asts = map sexprToAST exprs
                    let validAsts = sequence asts -- Check if all S-expressions were successfully converted to ASTs
                    case validAsts of
                        Nothing -> putStrLn "Error converting to AST"
                        Just astTrees -> evalMultiple astTrees initEnv -- Pass all valid ASTs for evaluation

        ["--repl"] -> repl

        [filename] -> do
            putStrLn $ "Reading file: " ++ filename
            input <- readFile filename
            putStrLn input

        _ -> putStrLn "Usage : <programme> [--repl | fichier]"

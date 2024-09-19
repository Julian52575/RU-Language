module Main (main) where

import System.Environment (getArgs)
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
               Left err -> putStrLn $ errorBundlePretty err
               Right expr -> do
                   let asts = map sexprToAST expr
                   case sequence asts of
                       Nothing -> putStrLn "Error converting to AST"
                       Just astTrees -> evalMultiple astTrees env
           repl env

-- Print the result of an evaluation
printResult :: Ast -> IO ()
printResult (AstInt n) = print n
printResult (AstBool True) = putStrLn "#t"
printResult (AstBool False) = putStrLn "NIL"
printResult (Lambda _ _) = putStrLn "#<procedure>"
printResult _ = putStrLn "Unsupported expression"

-- Optimized evalMultiple function
evalMultiple :: [Ast] -> Env -> IO ()
evalMultiple [] _ = return ()
evalMultiple (expr:rest) env = do
    case expr of
        Define var defExpr -> do
            let newEnv = evalDefine (Define var defExpr) env
            evalMultiple rest newEnv
        _ -> do
            case evalAST env expr of
                Nothing -> putStrLn "Error evaluating expression"
                Just result -> printResult result
            evalMultiple rest env

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
                    let validAsts = sequence asts
                    case validAsts of
                        Nothing -> putStrLn "Error converting to AST"
                        Just astTrees -> evalMultiple astTrees initEnv
        ["--repl"] -> repl initEnv
        [filename] -> do
            putStrLn $ "Reading file: " ++ filename
            input <- readFile filename
            let parsedExprs = parse parseSExprs "" input
            case parsedExprs of
                Left err -> putStrLn $ errorBundlePretty err
                Right exprs -> do
                    let asts = map sexprToAST exprs
                    let validAsts = sequence asts
                    case validAsts of
                        Nothing -> putStrLn "Error converting to AST"
                        Just astTrees -> evalMultiple astTrees initEnv
        _ -> putStrLn "Usage: <program> [--repl | filename]"

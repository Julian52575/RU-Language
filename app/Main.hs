module Main where

import System.Environment (getArgs)
import Parser (parseSExprs)
import AST (sexprToAST, Ast(..))
import Evaluator (evalAST, emptyEnv, evalDefine, Env)
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


evalMultiple :: [Ast] -> Env -> IO()
evalMultiple [] _ = return ()
evalMultiple (expr:rest) env = do
    let newEnv = evalDefine expr env  -- Si c'est une définition, on la stocke dans l'environnement
    case expr of
        Define _ _ -> do
            -- Si l'expression est une définition, on ne l'évalue pas
            evalMultiple rest newEnv  -- Continue avec le nouvel environnement
        _ -> do
            -- Si ce n'est pas une définition, on l'évalue
            case evalAST newEnv expr of
                Nothing -> putStrLn "Error evaluating expression"
                Just result -> print result
            evalMultiple rest newEnv  -- Continue avec le nouvel environnement


main :: IO ()
main = do
    args <- getArgs
    case args of

        [] -> do
            input <- getContents
            let parsedExprs = parse parseSExprs "" input
            print parsedExprs
            case parsedExprs of
                Left err -> putStrLn $ errorBundlePretty err
                Right exprs -> do
                    let asts = map sexprToAST exprs
                    let validAsts = sequence asts -- Vérifie que tout a été parsé correctement
                    case validAsts of
                        Nothing -> putStrLn "Error converting to AST"
                        Just astTrees -> evalMultiple astTrees emptyEnv -- Passe tous les ASTs
        ["--repl"] -> repl

        [filename] -> do
            putStrLn $ "Reading file: " ++ filename
            input <- readFile filename
            putStrLn input

        _ -> putStrLn "Usage : <programme> [--repl | fichier]"

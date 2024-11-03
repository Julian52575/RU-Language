{-# LANGUAGE OverloadedStrings #-}

module Parser.File (parseFromFile, parseFromString) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (runParser, errorBundlePretty)
import Parser.Utils (sc)
import Parser.Stmt (Stmt, stmt)
import Text.Megaparsec (many, eof)
import Opcode (compileAst)

parseFromString :: String -> Either String [Stmt]
parseFromString input = 
    let result = runParser (sc *> many stmt <* eof) "" input
    in case result of
        Left err -> Left $ errorBundlePretty err
        Right ast -> Right ast

parseFromFile :: FilePath -> FilePath -> IO ()
parseFromFile path out = do
    content <- TIO.readFile path
    let result = parseFromString (T.unpack content)
    case result of
        Left err -> putStrLn $ "Parse error:\n" ++ err
        Right ast -> do 
            compileAst ast out
            putStrLn $ "Output written to " ++ out

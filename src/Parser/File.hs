{-# LANGUAGE OverloadedStrings #-}

module Parser.File (parseFromFile, parseFromString) where

import System.IO (withFile, IOMode(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (ParseErrorBundle, runParser, errorBundlePretty)
import Data.Void (Void)
import Parser.Utils (sc)
import Parser.Stmt (Stmt, stmt)
import Text.Megaparsec (many, eof)

parseFromString :: String -> Either String [Stmt]
parseFromString input = 
    let result = runParser (sc *> many stmt <* eof) "" input
    in case result of
        Left err -> Left $ errorBundlePretty err
        Right ast -> Right ast

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
    content <- TIO.readFile path
    let result = parseFromString (T.unpack content)
    case result of
        Left err -> putStrLn $ "Parse error:\n" ++ err
        Right ast -> putStrLn $ "Successfully parsed AST:\n" ++ show ast

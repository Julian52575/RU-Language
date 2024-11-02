import System.Environment (getArgs)
import Parser.File (parseFromFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            putStrLn $ "Parsing file: " ++ filePath
            parseFromFile filePath
        _ -> putStrLn "Usage: <program> <file-path>"

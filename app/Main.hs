import System.Environment (getArgs)
import Parser.File (parseFromFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            putStrLn $ "Parsing file: " ++ filePath
            parseFromFile filePath "out.izly"
        [filePath1, filePath2] -> do
            putStrLn $ "Parsing file: " ++ filePath1
            parseFromFile filePath1 filePath2
        _ -> putStrLn "Usage: <program> <file-path> <output-path>"

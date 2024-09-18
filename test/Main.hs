module Main (main) where


data SExpr = SInt Int
    | SSymbol String
    | SList [SExpr]
    deriving Show


getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing


getInteger :: SExpr -> Maybe Int
getInteger (SInt i) = Just i
getInteger _ = Nothing


getList :: SExpr -> Maybe [SExpr]
getList (SList l) = Just l
getList _ = Nothing


printTree :: SExpr -> Maybe String
printTree (SSymbol s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (SInt n) = Just $ "a Number " ++ show n
printTree (SList []) = Just "an empty List"
printTree (SList (x:xs)) = do
    first <- printTree x
    rest <- mapM printTree xs
    let listContent = if null rest
                        then first
                        else first ++ concatMap (", " ++) rest
    Just $ "a List with " ++ listContent


data Ast
    = Define String Ast
    | AstInt Int
    | AstSym String
    | AstList [Ast]
    | Call String [Ast]
    deriving Show


sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt n) = Just (AstInt n)
sexprToAST (SSymbol s) = Just (AstSym s)

sexprToAST (SList [SSymbol "define", SSymbol var, expr]) = do
    astExpr <- sexprToAST expr
    Just (Define var astExpr)

sexprToAST (SList (SSymbol "define" : _)) = Nothing

sexprToAST (SList (SSymbol func : args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call func astArgs)

sexprToAST _ = Nothing


evalAST :: Ast -> Maybe Ast
evalAST (AstInt n) = Just (AstInt n)

evalAST (AstSym _) = Nothing

evalAST (Call func args) = do
    evalArgs <- mapM evalAST args
    intArgs <- mapM getInt evalArgs
    case func of
        "+" -> return $ AstInt (sum intArgs)
        "*" -> return $ AstInt (product intArgs)
        "-" -> return $ AstInt (foldl1 (-) intArgs)
        "/" -> if elem 0 (tail intArgs)
                then Nothing
                else return $ AstInt (foldl1 div intArgs)
        _   -> Nothing
evalAST _ = Nothing


getInt :: Ast -> Maybe Int
getInt (AstInt n) = Just n
getInt _ = Nothing


main :: IO ()
main = do
    putStrLn "Hello from Main!"
    input <- getContents
    print input

{-# LANGUAGE LambdaCase #-}

module Parser.Type (Type(..), parseType, pType) where

import Text.Megaparsec
import Parser.Utils (Parser, sc, lexeme, sym, identifier)
import Data.Void

-- Definition of the Type AST (Abstract Syntax Tree)
data Type
    = TInt               -- Integer type (int)
    | TString            -- String type (string)
    | TVoid              -- Void type (void)
    | TBool              -- Boolean type (bool)
    | TUnknown String    -- Unknown type (user-defined or undefined)
    | TTuple [Type]      -- Tuple type (type1, type2, ...)
    | TArray Type        -- Array type (type[])
    | TFunc [Type] Type  -- Function type ((param1, param2, ...) -> returnType)
    deriving (Show, Eq)

-- Parser for simple types (int, bool, string, void, or unknown types)
pSimpleType :: Parser Type
pSimpleType = lexeme $ choice
    [ TInt <$ sym "int"
    , TBool <$ sym "bool"
    , TString <$ sym "string"
    , TVoid <$ sym "void"
    , TUnknown <$> identifier
    ]

-- Parser for tuple types (type1, type2, ...) or a single type
pTupleType :: Parser Type
pTupleType = between (sym "(") (sym ")") (pType `sepBy1` sym ",") >>= \case
    [singleType] -> return singleType
    types        -> return $ TTuple types

-- Parser for function types ((param1, param2) -> returnType)
pFunctionType :: Parser Type
pFunctionType = do
    paramTypes <- between (sym "(") (sym ")") (pType `sepBy` sym ",")
    _ <- sym "->"
    TFunc paramTypes <$> pType

-- Parser for array types (type[]) with efficient recursion handling
pArrayType :: Type -> Parser Type
pArrayType baseType = option baseType (TArray <$> (sym "[]" *> pArrayType baseType))

-- Main parser for types (handles function types, tuples, simple types, and arrays)
pType :: Parser Type
pType = do
    baseType <- choice
        [ try pFunctionType
        , try pTupleType
        , pSimpleType
        ]
    pArrayType baseType <?> "type"

-- Main function to parse a type from a string (used externally)
parseType :: String -> Either (ParseErrorBundle String Void) Type
parseType = runParser (sc *> pType <* eof) ""

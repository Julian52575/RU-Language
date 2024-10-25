module Compiler.Type (
    Scope(..),
    OpCode(..),
    Compile(..),
    Function(..),
    CodingByte(..)
) where

import Data.Word (Word8, Word16)

data CodingByte = CbConst Int Int Int
    | CbVar Int Int
    deriving (Show, Eq)

data Scope = Scope {
    vars :: [String],
    function :: String,
    indexStart :: Int
} deriving (Show, Eq)

data OpCode = OpNoop 
    | OpPrint CodingByte
    | OpPrintLn CodingByte
    | OpCreateVar Int Int
    | OpSetVar Int CodingByte 
    | OpSetArg Int CodingByte
    | OpUnsetArg Int Int
    | OpSetReturn CodingByte
    | OpUnsetReturn Int
    | OpReturn
    | OpCall Int
    | OpJump Int
    | OpJumpCarry Int
    | OpJumpNotCarry Int
    | OpIfCarry Int
    | OpAdd CodingByte CodingByte
    | OpSub CodingByte CodingByte
    | OpDiv CodingByte CodingByte
    | OpMul CodingByte CodingByte
    | OpEq CodingByte CodingByte
    | OpNeq CodingByte CodingByte
    | OpMod CodingByte CodingByte
    deriving (Show, Eq)

data Function = Function {
    fIndex :: Int,
    fName :: String,
    fOffset :: Maybe Int,
    fSize :: Maybe Int
} deriving (Show, Eq)

data Compile = Compile {
    stringTable :: [String],
    functionTable :: [Function],
    globalScope :: Scope
} deriving (Show, Eq)

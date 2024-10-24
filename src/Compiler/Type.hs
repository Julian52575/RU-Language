module Compiler.Type (
    Scope(..),
    OpCode(..),
    Compile(..)
) where

import Data.Word (Word8, Word16)
import Compiler.Function (Function)

data Scope = Scope {
    vars :: [String],
    function :: String,
    indexStart :: Int
} deriving (Show, Eq)

data OpCode = OpNoop 
    | OpPrint Int
    | OpPrintLn Int
    | OpCreateVar Int Int
    | OpSetVar Int Int
    | OpSetArg Int Int
    | OpUnsetArg Int Int
    | OpSetReturn Int
    | OpUnsetReturn Int
    | OpReturn
    | OpCall Int
    | OpJump Int
    | OpJumpCarry Int
    | OpJumpNotCarry Int
    | OpIfCarry Int
    | OpAdd Int Int
    | OpSub Int Int
    | OpDiv Int Int
    | OpMul Int Int
    | OpEq Int Int
    | OpNeq Int Int
    | OpMod Int Int
    deriving (Show, Eq)

data Compile = Compile {
    stringTable :: [String],
    functionTable :: [Function],
    globalScope :: Scope
} deriving (Show, Eq)

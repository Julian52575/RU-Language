module Opcode (
    OpCode(..),
    Builtin(..),
    Instruction(..),
    buildInstructions,
    buildBuiltin
) where

import Data.Word (Word8, Word16)

data OpCode = OpCode {
    mnemonic :: Word16,
    operand1 :: [Word8],
    operand2 :: [Word8],
    operand3 :: [Word8],
    operand4 :: [Word8]
} deriving (Show, Eq)

data Builtin = Noop OpCode
            | Print OpCode
            | PrintLn OpCode
            deriving (Show, Eq)

data Instruction = CreateVar OpCode
                | SetVar OpCode
                | SetTmpVar OpCode
                | SetArg OpCode
                | UnsetArg OpCode
                | SetReturn OpCode
                | UnsetReturn OpCode
                | Return OpCode
                | Call OpCode
                | Jump OpCode
                | JumpCarry OpCode
                | JumpNotCarry OpCode
                | OpAdd OpCode
                | OpSub OpCode
                | OpDiv OpCode
                | OpMul OpCode
                | OpEq OpCode
                | OpNeq OpCode
                deriving (Show, Eq)

buildOpCode :: Word16 -> [Word8] -> [Word8] -> [Word8] -> [Word8] -> OpCode
buildOpCode m o1 o2 o3 o4 = OpCode {
    mnemonic = m,
    operand1 = o1,
    operand2 = o2,
    operand3 = o3,
    operand4 = o4
}

buildInstructions :: [Instruction]
buildInstructions = [
    CreateVar (buildOpCode 0x0100 [1] [1, 2] [0] [0]),
    SetVar (buildOpCode 0x0101 [2] [1, 2] [0] [0]),
    SetTmpVar (buildOpCode 0x0102 [1] [1] [0] [0]),
    SetArg (buildOpCode 0x0103 [1, 2] [1] [0] [0]),
    UnsetArg (buildOpCode 0x0104 [2] [1] [0] [0]),
    SetReturn (buildOpCode 0x0105 [1, 2] [0] [0] [0]),
    UnsetReturn (buildOpCode 0x0106 [2] [0] [0] [0]),
    Return (buildOpCode 0x0200 [0] [0] [0] [0]),
    Call (buildOpCode 0x0201 [1] [0] [0] [0]),
    Jump (buildOpCode 0x0202 [1] [0] [0] [0]),
    JumpCarry (buildOpCode 0x0203 [1] [0] [0] [0]),
    JumpNotCarry (buildOpCode 0x0204 [1] [0] [0] [0]),
    OpAdd (buildOpCode 0x0300 [2] [1, 2] [0] [0]),
    OpSub (buildOpCode 0x0301 [2] [1, 2] [0] [0]),
    OpDiv (buildOpCode 0x0302 [2] [1, 2] [0] [0]),
    OpMul (buildOpCode 0x0303 [2] [1, 2] [0] [0]),
    OpEq (buildOpCode 0x0304 [2] [1, 2] [0] [0]),
    OpNeq (buildOpCode 0x0305 [2] [1, 2] [0] [0])
    ]

buildBuiltin :: [Builtin]
buildBuiltin = [
    Noop (buildOpCode 0x0000 [0] [0] [0] [0]),
    Print (buildOpCode 0x00001 [2] [0] [0] [0]),
    PrintLn (buildOpCode 0x00002 [2] [0] [0] [0])
    ]

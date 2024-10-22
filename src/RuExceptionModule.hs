module RuExceptionModule where

newtype RuException = RuException String

ruExceptionInvalidProgramCounter :: RuException
ruExceptionInvalidProgramCounter = RuException "Invalid program counter"

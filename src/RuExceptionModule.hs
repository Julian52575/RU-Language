module RuExceptionModule where

import Data.Word
import Text.Printf (printf)

newtype RuException = RuException String
    deriving (Eq, Show)


ruExceptionInvalidProgramCounter :: RuException
ruExceptionInvalidProgramCounter = RuException "Invalid program counter"

ruExceptionBadChecksum :: RuException
ruExceptionBadChecksum = RuException "Checksum is invalid"

ruExceptionGenericFileError :: RuException
ruExceptionGenericFileError = RuException "Error on file"

ruExceptionInvalidCodeOffset :: RuException
ruExceptionInvalidCodeOffset = RuException "Code offset is invalid"

ruExceptionUnsupportedVersion :: RuException
ruExceptionUnsupportedVersion = RuException "Unsupported file version"

ruExceptionWrongFileFormat :: RuException
ruExceptionWrongFileFormat = RuException "Invalid file format"

ruExceptionBadFunctionTableCount :: RuException
ruExceptionBadFunctionTableCount = RuException "Invalid function table count"

ruExceptionIncompleteInstruction :: RuException
ruExceptionIncompleteInstruction = RuException "Instruction is incomplete."

ruExceptionUnknowInstruction :: Word8 -> Word8-> RuException
ruExceptionUnknowInstruction p i = RuException ("Unknow instruction 0x" ++ (printf "%02x" p) ++ (printf "-%02x" i) ++ ".")

ruExceptionInvalidCodingByte :: RuException
ruExceptionInvalidCodingByte = RuException "Invalid codingByte using 0x01 bit pair."

ruExceptionUnknowVariable :: Word32 -> RuException
ruExceptionUnknowVariable idd = RuException ("Unknow variable id: " ++ printf "0x%08x" idd)

ruExceptionVariableAlreadyExists :: Word32 -> RuException
ruExceptionVariableAlreadyExists idd = RuException ("Variable id already exists: " ++ printf "0x%08x" idd)

ruExceptionJumpOutOfScope :: RuException
ruExceptionJumpOutOfScope = RuException ("Trying to jump outside of current scope.")

ruExceptionJumpOutOfBound :: RuException
ruExceptionJumpOutOfBound = RuException ("Trying to jump outside of code.")

ruExceptionInvalidOperation :: RuException
ruExceptionInvalidOperation = RuException ("Invalid operation.")

ruExceptionDivByZero :: RuException
ruExceptionDivByZero = RuException ("Division by zero.")

ruExceptionUnknowArgument :: Word32 -> RuException
ruExceptionUnknowArgument i = RuException ("Unknow argument " ++ printf "0x%08x" i ++ ".")

ruExceptionAccessArgumentInFirstScope :: RuException
ruExceptionAccessArgumentInFirstScope = RuException ("Trying to access argument in first scope.")

module RuExceptionModule where

import Data.Word
import Numeric

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

ruExceptionUnknowInstruction :: RuException
ruExceptionUnknowInstruction = RuException "Unknow instruction."

ruExceptionUnknowOpcode :: Word8 -> Word8 -> RuException
ruExceptionUnknowOpcode p i = RuException ("Unknow opcode 0x" ++ (showHex p "??") ++ (showHex i "??"))

ruExceptionInvalidCodingByte :: RuException
ruExceptionInvalidCodingByte = RuException "Invalid codingByte using 0x01 bit pair."

ruExceptionUnknowVariable :: Word32 -> RuException
ruExceptionUnknowVariable idd = RuException ("Unknow variable id: " ++ showHex idd "??")

ruExceptionVariableAlreadyExists :: Word32 -> RuException
ruExceptionVariableAlreadyExists idd = RuException ("Variable id already exists: " ++ showHex idd "??")

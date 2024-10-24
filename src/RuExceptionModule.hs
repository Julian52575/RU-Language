module RuExceptionModule where

import Data.Word
import Numeric

newtype RuException = RuException String

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

ruExceptionIncompleteOpcode :: Word32 -> RuException
ruExceptionIncompleteOpcode offset32 = RuException exc
    where
    offsetStr = showHex offset32 "0x??"
    exc = "Opcode is complete at pc:" ++ offsetStr

module RuExceptionModule where

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


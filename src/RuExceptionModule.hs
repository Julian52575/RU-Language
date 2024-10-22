module RuExceptionModule where

newtype RuException = RuException String

ruExceptionInvalidProgramCounter :: RuException
ruExceptionInvalidProgramCounter = RuException "Invalid program counter"

ruExceptionBadChecksum :: RuException
ruExceptionBadChecksum = RuException "Checksum is invalid"

ruExceptionUnsupportedVersion :: RuException
ruExceptionUnsupportedVersion = RuException "Unsupported file version"

ruExceptionWrongFileFormat :: RuException
ruExceptionWrongFileFormat = RuException "Invalid file format"


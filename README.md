# Ru Virtual-Machine
A simple, secure and yet efficient virtual machine to safely run your programs.

## Supports:
-  Integers
-  Strings
-  Function call

## Safety features:  
- Control flow Integrity
- Limit recursion depth
- No pointers
- Automatic memory allocation and free
- Explicit errors for developers

## Installation:
Download the ru_vm binary from the latest release.

## Usage:
```
  ./ru_vm ruExecutable (--dump)
  options:
    --dump   =  print everything about the binary.

```
## For developers:
Find more details about the ru executable format (ruef) in the developer_doc folder.

## Design philosophy
The RU executable format aims to be secure, easily expendable and reusable.  
As such, it is easy to add data type and either update the default instructions or provide new ones. See 3 section bellow.
### Inspiration
Taking inspiration from x86_64 assembly, java and lua opcode.    
### Security features
The format allows many security features such as:
-  Control-flow _thanks to function size_
-  File integrety _checksum_
### Expending the Ru Executable Format for your needs
Here is a step-by-step guide:
1. Familiarize yourself with the format by reading the documentation in developer_doc.
2. If you want to provide a new data type:
   - Update the `RuVariableValue` data in `src/RuVariableModule.hs`.
   - Provide a new equation typed `:: Word8` to provide an id of your new type.
   - Update the `getRuVariableValueAsString` fonction to provide a way to print your type.
3. If you want to provide a new instruction:
   - In the `src/RuInstructionModule.hs`file, create a new equation type `:: RuInstruction` to create a record of your new instruction.
   - Create a new function typed `RuVmInfo -> RuVmState -> Either RuException RuVmState` to execute your instruction.
   - Update the list provided in the `src/RuInstructionHelperModule.hs` to include your new RuInstruction.



























module Compiler.CodingByte (
    getCodingByte
) where

import Compiler.Type (Scope(..), CodingByte(..))
import Parser.AST (Expr(..))
import Compiler.CreateVar (getIndexFromStrTable)

-- get the coding byte from an expression
getCodingByte :: Expr -> Scope -> [String] -> CodingByte
getCodingByte (LitInt int) _ _ = CbConst 0xA0 0x01 int
getCodingByte (LitString str) _ strTable = CbConst 0xA0 0x02 (getIndexFromStrTable strTable str)
getCodingByte (Var var) scope _ = CbConst 0xB0 0x01 (getIndexFromStrTable (vars scope) var)
getCodingByte _ _ _ = CbConst 0xA0 0x01 0x00

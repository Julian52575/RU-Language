module Compiler.CodingByte (
    getCodingByte
) where

import Compiler.Type (Scope(..), CodingByte(..))
import Parser.AST (Expr(..))
import Compiler.CreateVar (getIndexFromStrTable)

-- get the coding byte from an expression
getCodingByte :: Expr -> Scope -> [String] -> CodingByte
getCodingByte (LitInt int) _ _ = CbConst 0x02 0x01 int
getCodingByte (LitString str) _ strTable = CbConst 0x02 0x02 (getIndexFromStrTable strTable str)
getCodingByte (Var var) scope _ = CbVar 0x03 (getIndexFromStrTable (vars scope) var)
getCodingByte _ _ _ = CbConst 0x02 0x01 0x00

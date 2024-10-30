-- test/Spec.hs
import Test.Hspec
import qualified TypeSpec
import qualified ExprSpec
import qualified StmtSpec
import qualified PatternSpec
import qualified FunctionSpec

import qualified Compiler.StringComp as StringComp
import qualified Compiler.FunctionComp as FunctionComp
import qualified Compiler.CreateVarComp as CreateVarComp
import qualified Compiler.BinArithComp as BinArithComp
import qualified Compiler.CompilerComp as CompilerComp

main :: IO ()
main = hspec $ do
  describe "Type tests" TypeSpec.spec
  describe "Expression tests" ExprSpec.spec
  describe "Operator tests" StmtSpec.spec
  describe "Pattern tests" PatternSpec.spec
  describe "Statement tests" StmtSpec.spec
  describe "Function tests" FunctionSpec.spec

  describe "String Table tests" StringComp.spec
  describe "Function Table tests" FunctionComp.spec
  describe "CreateVar opcode" CreateVarComp.spec
  describe "BinAritmetic opcode" BinArithComp.spec
  describe "Opcode from statements" CompilerComp.spec

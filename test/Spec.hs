-- test/Spec.hs
import Test.Hspec
import qualified TypeSpec
import qualified ExprSpec
import qualified StmtSpec
import qualified PatternSpec
import qualified FunctionSpec

main :: IO ()
main = hspec $ do
  describe "Type tests" TypeSpec.spec
  describe "Expression tests" ExprSpec.spec
  describe "Operator tests" StmtSpec.spec
  describe "Pattern tests" PatternSpec.spec
  describe "Statement tests" StmtSpec.spec
  describe "Function tests" FunctionSpec.spec

-- test/Spec.hs
import Test.Hspec
import qualified TypeSpec
import qualified ExprSpec
import qualified StmtSpec

main :: IO ()
main = hspec $ do
  describe "Type tests" TypeSpec.spec
  describe "Expression tests" ExprSpec.spec
  describe "Operator tests" StmtSpec.spec

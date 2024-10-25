import Test.Hspec

import RuVmTests
import RuFormatTests
import RuInstructionsTests
import RuOperandTests

main :: IO ()
main = hspec $ do
    describe "RuVM tests" RuVmTests.spec
    describe "RuFormat tests" RuFormatTests.spec
    describe "RuInstruction tests" RuInstructionsTests.spec
    describe "RuOperand tests" RuOperandTests.spec

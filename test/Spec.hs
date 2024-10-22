import Test.Hspec

import RuVmTests
import RuFormatTests

main :: IO ()
main = hspec $ do
    describe "RuVM tests" RuVmTests.spec
    describe "RuFormat tests" RuFormatTests.spec

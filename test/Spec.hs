import Test.Hspec

import RuVmTests

main :: IO ()
main = hspec $ do
    describe "RuVM tests" RuVmTests.spec

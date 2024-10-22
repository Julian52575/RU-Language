module RuFormatTests where

import Test.Hspec

import RuFormatModule
import RuVmModule

spec :: Spec
spec = do
    describe "Combine n WordX into a WordY" $ do
        it "Combine 2 Word8 into a Word16" $ do
            (word82ToWord16 0xab 0xcd) `shouldBe` 0xabcd
        it "Combine 4 Word8 into a Word32" $ do
            (word84ToWord32 0x89 0xab 0xcd 0xef) `shouldBe` 0x89abcdef

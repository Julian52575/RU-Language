module RuOperandTests where

import Test.Hspec

import RuOperandModule
{--
data RuOperand =
    RuOperandNone       |
    RuOperandUnused     |
    RuOperandConstant   |
    RuOperandVariableId
    deriving (Eq, Show)

ruOperandToWord8 :: ruOperand -> Word8
--}

spec :: Spec
spec = do
    describe "ruOperandToWord8" $ do
        it "ruOperandNone" $ do
            ruOperandToWord8 RuOperandNone `shouldBe` 0x00
        it "ruOperandUnused" $ do
            ruOperandToWord8 RuOperandUnused `shouldBe` 0x01
        it "ruOperandConstant" $ do
            ruOperandToWord8 RuOperandConstant `shouldBe` 0x02
        it "ruOperandVariableId" $ do
            ruOperandToWord8 RuOperandVariableId `shouldBe` 0x03


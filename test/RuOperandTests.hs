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
--codingByteToRuOperand :: Word8 -> [RuOperand]
    describe "codingByteToRuOperand" $ do
        it "Convert all RuOperandVariableId" $ do
            codingByteToRuOperand 0xFF `shouldBe` [RuOperandVariableId, RuOperandVariableId, RuOperandVariableId, RuOperandVariableId]
        it "Convert all RuOperandConstant" $ do
            codingByteToRuOperand 0xAA `shouldBe` [RuOperandConstant, RuOperandConstant, RuOperandConstant, RuOperandConstant]
        it "Convert all RuOperandUnused" $ do
            codingByteToRuOperand 0x55 `shouldBe` [RuOperandUnused, RuOperandUnused, RuOperandUnused, RuOperandUnused]
        it "Convert all RuOperandNone" $ do
            codingByteToRuOperand 0x00 `shouldBe` [RuOperandNone, RuOperandNone, RuOperandNone, RuOperandNone]
--codingByteToOperandsSize :: Word8 -> Word32 --TODO
    describe "codingByteToOperandsSize" $ do
        it "Get size of mixed RuOperand" $ do --11 10 01 00
            codingByteToOperandsSize 0xE4 `shouldBe` (4 + 4 + 0 + 0)
        it "Get size of all RuOperandVariableId" $ do
            codingByteToOperandsSize 0xFF `shouldBe` (4 + 4 + 4 + 4)
        it "Get size of all RuOperandConstant" $ do
            codingByteToOperandsSize 0xAA `shouldBe` (4 + 4 + 4 + 4)
        it "Get size of all RuOperandUnused" $ do
            codingByteToOperandsSize 0x55 `shouldBe` (0 + 0 + 0 + 0)
        it "Get size of all RuOperandNone" $ do
            codingByteToOperandsSize 0x00 `shouldBe` (0 + 0 + 0 + 0)

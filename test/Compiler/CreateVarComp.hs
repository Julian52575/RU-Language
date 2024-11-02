module Compiler.CreateVarComp (
    spec
) where

import Test.Hspec
import Parser.AST (Stmt(..), Expr(..), RangeType(..))
import Parser.Pattern (Pattern(..))
import Parser.Type (Type(..))
import Compiler.Type (OpCode(..))
import Compiler.CreateVar (getCreateVar)

spec :: Spec
spec = do
  describe "Create Var opcodes" $ do


    it "returns OpCode for a single LetStmt with TInt" $
      let input = [LetStmt "x" (Just TInt) (LitInt 4)]
          strTable = []
          expected = [(OpCreateVar 0x01 4, "x")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCode for a single LetStmt with TString" $
      let input = [LetStmt "x" (Just TString) (LitString "hello")]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x02 0, "x")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for multiple LetStmts" $
      let input = [LetStmt "x" (Just TInt) (LitInt 4), LetStmt "y" (Just TString) (LitString "hello")]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x01 4, "x"), (OpCreateVar 0x02 0, "y")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for nested IfStmt" $
      let input = [IfStmt (LitBool True) (LetStmt "x" (Just TInt) (LitInt 4)) (Just (LetStmt "y" (Just TString) (LitString "hello")))]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x01 4, "x"), (OpCreateVar 0x02 0, "y")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for nested IfStmt" $
      let input = [IfStmt (LitBool True) 
                          (IfStmt (LitBool False) 
                                  (LetStmt "x" (Just TInt) (LitInt 4)) 
                                  (Just (LetStmt "y" (Just TString) (LitString "hello")))) 
                          (Just (LetStmt "z" (Just TInt) (LitInt 5)))]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x01 4, "x"), (OpCreateVar 0x02 0, "y"), (OpCreateVar 0x01 5, "z")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for BlockStmt" $
      let input = [BlockStmt [LetStmt "x" (Just TInt) (LitInt 4), LetStmt "y" (Just TString) (LitString "hello")]]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x01 4, "x"), (OpCreateVar 0x02 0, "y")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for ForRangeStmt" $
      let input = [ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive (Just (LitInt 1)) [LetStmt "x" (Just TString) (LitString "hello")]]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x02 0, "x")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for WhileStmt" $
      let input = [WhileStmt (LitBool True) [LetStmt "x" (Just TString) (LitString "hello")]]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x02 0, "x")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for DoWhileStmt" $
      let input = [DoWhileStmt [LetStmt "x" (Just TString) (LitString "hello")] (LitBool True)]
          strTable = ["hello"]
          expected = [(OpCreateVar 0x02 0, "x")]
      in getCreateVar input strTable `shouldBe` expected

    it "returns OpCodes for MatchStmt" $
      let input = [MatchStmt (LitInt 1) [(PatLitString "pattern", LetStmt "x" (Just TString) (LitString "hello"))]]
          strTable = ["pattern", "hello"]
          expected = [(OpCreateVar 0x02 1, "x")]
      in getCreateVar input strTable `shouldBe` expected
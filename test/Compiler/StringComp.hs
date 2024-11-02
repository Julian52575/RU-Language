module Compiler.StringComp (
    spec
) where

import Test.Hspec
import Compiler.String (getStringTable)
import Parser.AST
import Parser.Type (Type(..))
import Parser.Pattern (Pattern(..))

spec :: Spec
spec = do
  describe "String Table" $ do

    it "get variable name" $
      let input = [LetStmt "x" (Just TInt) (LitInt 4)]
      in getStringTable input `shouldBe` ["x"]

    it "get cosnt string in LetStmt" $
      let input = [LetStmt "x" (Just TString) (LitString "hello")]
      in getStringTable input `shouldBe` ["x", "hello"]

    it "gets multiple variable names" $
      let input = [LetStmt "x" (Just TInt) (LitInt 4), LetStmt "y" (Just TInt) (LitInt 5)]
      in getStringTable input `shouldBe` ["x", "y"]

    it "gets variable and const string" $
      let input = [LetStmt "x" (Just TString) (LitString "hello"), LetStmt "y" (Just TInt) (LitInt 5)]
      in getStringTable input `shouldBe` ["x", "hello", "y"]

    it "gets nested expressions" $
      let input = [LetStmt "x" (Just TInt) (BinArith Add (LitInt 2) (LitInt 3))]
      in getStringTable input `shouldBe` ["x"]

    it "gets function names" $
      let input = [FuncDeclStmt "f" [] TInt Nothing, FuncDeclStmt "g" [] TInt Nothing]
      in getStringTable input `shouldBe` ["f", "g"]

    it "gets function arguments" $
      let input = [FuncDeclStmt "f" [("arg1", TInt, Nothing), ("arg2", TString, Nothing)] TInt Nothing]
      in getStringTable input `shouldBe` ["f", "arg1", "arg2"]

    it "gets strings in expressions" $
      let input = [ExprStmt (LitString "hello")]
      in getStringTable input `shouldBe` ["hello"]

    it "gets strings in patterns" $
      let input = [MatchStmt (LitInt 1) [(PatLitString "pattern", ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["pattern"]

    it "gets strings in nested statements" $
      let input = [BlockStmt [LetStmt "x" (Just TString) (LitString "hello"), LetStmt "y" (Just TInt) (LitInt 5)]]
      in getStringTable input `shouldBe` ["x", "hello", "y"]

    it "gets strings in if statements" $
      let input = [IfStmt (LitBool True) (LetStmt "x" (Just TString) (LitString "hello")) (Just (LetStmt "y" (Just TInt) (LitInt 5)))]
      in getStringTable input `shouldBe` ["x", "hello", "y"]

    it "gets strings in ForRangeStmt with range and body" $
      let input = [ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive (Just (LitInt 1)) [LetStmt "x" (Just TString) (LitString "hello")]]
      in getStringTable input `shouldBe` ["i", "x", "hello"]

    it "gets strings in ForRangeStmt with range and no body" $
      let input = [ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive Nothing []]
      in getStringTable input `shouldBe` ["i"]

    it "gets strings in ForRangeStmt with inclusive range and body" $
      let input = [ForRangeStmt "i" (LitInt 0) (LitInt 10) Inclusive (Just (LitInt 1)) [LetStmt "x" (Just TString) (LitString "hello")]]
      in getStringTable input `shouldBe` ["i", "x", "hello"]

    it "gets strings in ForRangeStmt with nested statements" $
      let input = [ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive (Just (LitInt 1)) [LetStmt "x" (Just TString) (LitString "hello"), LetStmt "y" (Just TInt) (LitInt 5)]]
      in getStringTable input `shouldBe` ["i", "x", "hello", "y"]

    it "gets strings in ForRangeStmt with complex range expressions" $
      let input = [ForRangeStmt "i" (BinArith Add (LitInt 0) (LitInt 1)) (BinArith Subtract (LitInt 10) (LitInt 1)) Exclusive (Just (LitInt 1)) [LetStmt "x" (Just TString) (LitString "hello")]]
      in getStringTable input `shouldBe` ["i", "x", "hello"]

    it "gets strings in while statements" $
      let input = [WhileStmt (LitBool True) [LetStmt "x" (Just TString) (LitString "hello")]]
      in getStringTable input `shouldBe` ["x", "hello"]

    it "gets strings in DoWhileStmt" $
      let input = [DoWhileStmt [LetStmt "x" (Just TString) (LitString "hello")] (LitBool True)]
      in getStringTable input `shouldBe` ["x", "hello"]

    it "gets strings in DoWhileStmt with nested statements" $
      let input = [DoWhileStmt [LetStmt "x" (Just TString) (LitString "hello"), LetStmt "y" (Just TInt) (LitInt 5)] (LitBool True)]
      in getStringTable input `shouldBe` ["x", "hello", "y"]

    it "gets strings in DoWhileStmt with complex condition" $
      let input = [DoWhileStmt [LetStmt "x" (Just TString) (LitString "hello")] (BinLogic And (LitBool True) (LitBool False))]
      in getStringTable input `shouldBe` ["x", "hello"]

    it "gets strings in function bodies" $
      let input = [FuncDeclStmt "f" [] TInt (Just (LetStmt "x" (Just TString) (LitString "hello")))]
      in getStringTable input `shouldBe` ["f", "x", "hello"]

    it "gets function name and arguments in FuncDeclStmt" $
      let input = [FuncDeclStmt "f" [("arg1", TInt, Nothing), ("arg2", TString, Nothing)] TInt Nothing]
      in getStringTable input `shouldBe` ["f", "arg1", "arg2"]

    it "gets function name, arguments, and body in FuncDeclStmt" $
      let input = [FuncDeclStmt "f" [("arg1", TInt, Nothing), ("arg2", TString, Nothing)] TInt (Just (LetStmt "x" (Just TString) (LitString "hello")))]
      in getStringTable input `shouldBe` ["f", "arg1", "arg2", "x", "hello"]

    it "gets function name and arguments with default values in FuncDeclStmt" $
      let input = [FuncDeclStmt "f" [("arg1", TInt, Just (LitInt 1)), ("arg2", TString, Just (LitString "default"))] TInt Nothing]
      in getStringTable input `shouldBe` ["f", "arg1", "arg2", "default"]

    it "gets function name, arguments with default values, and body in FuncDeclStmt" $
      let input = [FuncDeclStmt "f" [("arg1", TInt, Just (LitInt 1)), ("arg2", TString, Just (LitString "default"))] TInt (Just (LetStmt "x" (Just TString) (LitString "hello")))]
      in getStringTable input `shouldBe` ["f", "arg1", "arg2", "default", "x", "hello"]

    it "gets function name and empty body in FuncDeclStmt" $
      let input = [FuncDeclStmt "f" [("arg1", TInt, Nothing), ("arg2", TString, Nothing)] TInt (Just (BlockStmt []))]
      in getStringTable input `shouldBe` ["f", "arg1", "arg2"]

    it "gets strings in MatchStmt with PatLitString" $
      let input = [MatchStmt (LitInt 1) [(PatLitString "pattern", ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["pattern"]

    it "gets strings in MatchStmt with PatVar" $
      let input = [MatchStmt (LitInt 1) [(PatVar "varPattern", ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["varPattern"]

    it "gets strings in MatchStmt with PatOr" $
      let input = [MatchStmt (LitInt 1) [(PatOr [PatLitString "pattern1", PatLitString "pattern2"], ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["pattern1", "pattern2"]

    it "gets strings in MatchStmt with PatRange" $
      let input = [MatchStmt (LitInt 1) [(PatRange (PatLitString "start") (PatLitString "end"), ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["start", "end"]

    it "gets strings in MatchStmt with PatRangeInclusive" $
      let input = [MatchStmt (LitInt 1) [(PatRangeInclusive (PatLitString "start") (PatLitString "end"), ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["start", "end"]

    it "gets strings in MatchStmt with PatTuple" $
      let input = [MatchStmt (LitInt 1) [(PatTuple [PatLitString "elem1", PatLitString "elem2"], ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["elem1", "elem2"]

    it "gets strings in MatchStmt with PatArray" $
      let input = [MatchStmt (LitInt 1) [(PatArray [PatLitString "elem1", PatLitString "elem2"], ExprStmt (LitInt 2))]]
      in getStringTable input `shouldBe` ["elem1", "elem2"]

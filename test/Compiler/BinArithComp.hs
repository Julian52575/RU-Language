module Compiler.BinArithComp (
    spec
) where

import Test.Hspec
import Parser.AST
import Compiler.Type
import Compiler.BinArith (compileBinArith)

spec :: Spec
spec = do
  describe "Arithmetic operation to opcodes" $ do


    it "compiles a simple addition" $
      let expr = BinArith Add (LitInt 1) (LitInt 2)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpAdd (CbConst 0xA0 0x01 1) (CbConst 0xA0 0x01 2), OpUnsetReturn 0]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles a simple subtraction" $
      let expr = BinArith Subtract (LitInt 3) (LitInt 1)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpSub (CbConst 0xA0 0x01 3) (CbConst 0xA0 0x01 1), OpUnsetReturn 0]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles a simple multiplication" $
      let expr = BinArith Multiply (LitInt 2) (LitInt 3)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpMul (CbConst 0xA0 0x01 2) (CbConst 0xA0 0x01 3), OpUnsetReturn 0]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles a simple division" $
      let expr = BinArith Divide (LitInt 6) (LitInt 2)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpDiv (CbConst 0xA0 0x01 6) (CbConst 0xA0 0x01 2), OpUnsetReturn 0]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles a simple modulo" $
      let expr = BinArith Modulo (LitInt 5) (LitInt 3)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpMod (CbConst 0xA0 0x01 5) (CbConst 0xA0 0x01 3), OpUnsetReturn 0]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested addition and subtraction" $
      let expr = BinArith Add (BinArith Subtract (LitInt 5) (LitInt 3)) (LitInt 2)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpSub (CbConst 160 1 5) (CbConst 160 1 3),OpUnsetReturn 1,OpAdd (CbConst 176 1 1) (CbConst 160 1 2),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested multiplication and division" $
      let expr = BinArith Multiply (BinArith Divide (LitInt 8) (LitInt 2)) (LitInt 3)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpDiv (CbConst 160 1 8) (CbConst 160 1 2),OpUnsetReturn 1,OpMul (CbConst 176 1 1) (CbConst 160 1 3),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested addition and multiplication" $
      let expr = BinArith Add (BinArith Multiply (LitInt 2) (LitInt 3)) (LitInt 4)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpMul (CbConst 160 1 2) (CbConst 160 1 3),OpUnsetReturn 1,OpAdd (CbConst 176 1 1) (CbConst 160 1 4),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested expressions with variables" $
      let expr = BinArith Add (BinArith Multiply (Var "x") (LitInt 3)) (LitInt 4)
          scope = Scope ["x"] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpMul (CbConst 176 1 0) (CbConst 160 1 3),OpUnsetReturn 1,OpAdd (CbConst 176 1 1) (CbConst 160 1 4),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested expressions with strings" $
      let expr = BinArith Add (BinArith Multiply (LitString "hello") (LitInt 3)) (LitInt 4)
          scope = Scope [] "" 0
          compile = Compile ["hello"] [] scope
          expected = [OpCreateVar 2 0,OpMul (CbConst 160 1 0) (CbConst 160 1 3),OpUnsetReturn 1,OpAdd (CbConst 176 1 1) (CbConst 160 1 4),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles deeply nested addition and multiplication" $
      let expr = BinArith Add (BinArith Multiply (BinArith Add (LitInt 1) (LitInt 2)) (LitInt 3)) (LitInt 4)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpCreateVar 1 0,OpAdd (CbConst 160 1 1) (CbConst 160 1 2),OpUnsetReturn 2,OpMul (CbConst 176 1 2) (CbConst 160 1 3),OpUnsetReturn 1,OpUnsetVar 2,OpAdd (CbConst 176 1 1) (CbConst 160 1 4),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested addition, subtraction, and multiplication" $
      let expr = BinArith Add (BinArith Subtract (BinArith Multiply (LitInt 2) (LitInt 3)) (LitInt 1)) (LitInt 4)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpCreateVar 1 0,OpMul (CbConst 160 1 2) (CbConst 160 1 3),OpUnsetReturn 2,OpSub (CbConst 176 1 2) (CbConst 160 1 1),OpUnsetReturn 1,OpUnsetVar 2,OpAdd (CbConst 176 1 1) (CbConst 160 1 4),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested addition, division, and modulo" $
      let expr = BinArith Add (BinArith Divide (BinArith Modulo (LitInt 10) (LitInt 3)) (LitInt 2)) (LitInt 4)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpCreateVar 1 0,OpMod (CbConst 160 1 10) (CbConst 160 1 3),OpUnsetReturn 2,OpDiv (CbConst 176 1 2) (CbConst 160 1 2),OpUnsetReturn 1,OpUnsetVar 2,OpAdd (CbConst 176 1 1) (CbConst 160 1 4),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested expressions with multiple variables" $
      let expr = BinArith Add (BinArith Multiply (Var "x") (Var "y")) (Var "z")
          scope = Scope ["x", "y", "z"] "" 0
          compile = Compile [] [] scope
          expected = [OpCreateVar 1 0,OpMul (CbConst 176 1 0) (CbConst 176 1 1),OpUnsetReturn 1,OpAdd (CbConst 176 1 1) (CbConst 176 1 2),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles nested expressions with mixed types" $
      let expr = BinArith Add (BinArith Multiply (LitInt 2) (Var "x")) (LitString "hello")
          scope = Scope ["x"] "" 0
          compile = Compile ["hello"] [] scope
          expected = [OpCreateVar 1 0,OpMul (CbConst 160 1 2) (CbConst 176 1 0),OpUnsetReturn 1,OpAdd (CbConst 176 1 1) (CbConst 160 1 0),OpUnsetReturn 0,OpUnsetVar 1]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

    it "compiles deeply nested mixed expressions" $
      let expr = BinArith Add (BinArith Multiply (BinArith Add (Var "x") (LitInt 2)) (LitString "hello")) (BinArith Subtract (LitInt 5) (Var "y"))
          scope = Scope ["x", "y"] "" 0
          compile = Compile ["hello"] [] scope
          expected = [OpCreateVar 2 0,OpCreateVar 1 0,OpCreateVar 1 0,OpAdd (CbConst 176 1 0) (CbConst 160 1 2),OpUnsetReturn 3,OpMul (CbConst 176 1 3) (CbConst 160 1 0),OpUnsetReturn 2,OpUnsetVar 3,OpSub (CbConst 160 1 5) (CbConst 176 1 1),OpUnsetReturn 3,OpAdd (CbConst 176 1 1) (CbConst 176 1 2),OpUnsetReturn 0,OpUnsetVar 1,OpUnsetVar 2]
      in compileBinArith expr scope compile 0 0 `shouldBe` expected

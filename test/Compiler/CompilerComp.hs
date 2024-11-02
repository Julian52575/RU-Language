module Compiler.CompilerComp (
    spec
) where

import Test.Hspec
import Compiler.Compile
import Parser.Type
import Parser.AST
import Compiler.Type

spec :: Spec
spec = do
  describe "Compile module functions" $ do


    it "compiles a simple LetStmt" $
      let stmt = LetStmt "x" (Just TInt) (LitInt 4)
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = []
      in compileStmt stmt scope compile `shouldBe` expected

    it "compiles a BlockStmt" $
      let stmt = BlockStmt [LetStmt "x" (Just TInt) (LitInt 4), LetStmt "y" (Just TString) (LitString "hello")]
          scope = Scope [] "" 0
          compile = Compile ["hello"] [] scope
          expected = []
      in compileStmt stmt scope compile `shouldBe` expected

    it "compiles an IfStmt with else" $
      let stmt = IfStmt (LitBool True) (LetStmt "x" (Just TInt) (LitInt 4)) (Just (LetStmt "y" (Just TString) (LitString "hello")))
          scope = Scope [] "" 0
          compile = Compile ["hello"] [] scope
          expected = [OpEq (CbConst 160 1 1) (CbConst 160 1 1),OpJump 0]
      in compileStmt stmt scope compile `shouldBe` expected

    it "compiles an IfStmt without else" $
      let stmt = IfStmt (LitBool True) (LetStmt "x" (Just TInt) (LitInt 4)) Nothing
          scope = Scope [] "" 0
          compile = Compile [] [] scope
          expected = [OpEq (CbConst 160 1 1) (CbConst 160 1 1),OpJump 0]
      in compileStmt stmt scope compile `shouldBe` expected

    it "compiles a simple function" $
      let stmt = FuncDeclStmt "f" [("x", TInt, Nothing)] TInt (Just (ReturnStmt (Just (LitInt 4))))
          compile = Compile [] [] (Scope [] "" 0)
          expected = [OpCreateVar 1 0, OpUnsetArg 1 0, OpSetTmp 1 (CbConst 160 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a global statement" $
      let stmt = BlockStmt [LetStmt "x" (Just TInt) (LitInt 4)]
          compile = Compile [] [] (Scope [] "" 0)
          expected = [OpCreateVar 1 4, OpSetReturn 1 (CbConst 160 1 0), OpReturn]
      in compileGlobal stmt compile False `shouldBe` expected

    it "compiles a main function with int return type" $
      let stmt = FuncDeclStmt "main" [] TInt (Just (ReturnStmt (Just (LitInt 0))))
          compile = Compile [] [] (Scope [] "" 0)
          expected = [OpSetTmp 1 (CbConst 160 1 0), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a main function with void return type" $
      let stmt = FuncDeclStmt "main" [] TVoid (Just (ReturnStmt Nothing))
          compile = Compile [] [] (Scope [] "" 0)
          expected = []
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested addition and multiplication" $
      let stmt = FuncDeclStmt "f" [("x", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Add (BinArith Multiply (LitInt 2) (LitInt 3)) (LitInt 4)))))
          compile = Compile [] [] (Scope [] "" 0)
          expected = [OpCreateVar 1 0, OpUnsetArg 1 0, OpCreateVar 1 0, OpCreateVar 1 0, OpMul (CbConst 160 1 2) (CbConst 160 1 3), OpUnsetReturn 2, OpAdd (CbConst 176 1 2) (CbConst 160 1 4), OpUnsetReturn 1, OpUnsetVar 2, OpSetTmp 1 (CbConst 176 1 1), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested subtraction and division" $
      let stmt = FuncDeclStmt "g" [("y", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Subtract (BinArith Divide (LitInt 8) (LitInt 2)) (LitInt 1)))))
          compile = Compile [] [] (Scope [] "" 0)
          expected = [OpCreateVar 1 0, OpUnsetArg 1 0, OpCreateVar 1 0, OpCreateVar 1 0, OpDiv (CbConst 160 1 8) (CbConst 160 1 2), OpUnsetReturn 2, OpSub (CbConst 176 1 2) (CbConst 160 1 1), OpUnsetReturn 1, OpUnsetVar 2, OpSetTmp 1 (CbConst 176 1 1), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested addition, subtraction, and multiplication" $
      let stmt = FuncDeclStmt "h" [("z", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Add (BinArith Subtract (BinArith Multiply (LitInt 2) (LitInt 3)) (LitInt 1)) (LitInt 4)))))
          compile = Compile [] [] (Scope [] "" 0)
          expected = [OpCreateVar 1 0, OpUnsetArg 1 0, OpCreateVar 1 0, OpCreateVar 1 0, OpCreateVar 1 0, OpMul (CbConst 160 1 2) (CbConst 160 1 3), OpUnsetReturn 3, OpSub (CbConst 176 1 3) (CbConst 160 1 1), OpUnsetReturn 2, OpUnsetVar 3, OpAdd (CbConst 176 1 2) (CbConst 160 1 4), OpUnsetReturn 1, OpUnsetVar 2, OpSetTmp 1 (CbConst 176 1 1), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested expressions and variables" $
      let stmt = FuncDeclStmt "k" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Add (BinArith Multiply (Var "a") (Var "b")) (LitInt 4)))))
          compile = Compile [] [] (Scope ["a", "b"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpMul (CbConst 176 1 0) (CbConst 176 1 1), OpUnsetReturn 5, OpAdd (CbConst 176 1 5) (CbConst 160 1 4), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with deeply nested mixed expressions" $
      let stmt = FuncDeclStmt "m" [("x", TInt, Nothing), ("y", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Add (BinArith Multiply (BinArith Add (Var "x") (LitInt 2)) (LitString "hello")) (BinArith Subtract (LitInt 5) (Var "y"))))))
          compile = Compile ["hello"] [] (Scope ["x", "y"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 2 0, OpCreateVar 1 0, OpCreateVar 1 0, OpAdd (CbConst 176 1 0) (CbConst 160 1 2), OpUnsetReturn 6, OpMul (CbConst 176 1 6) (CbConst 160 1 0), OpUnsetReturn 5, OpUnsetVar 6, OpSub (CbConst 160 1 5) (CbConst 176 1 1), OpUnsetReturn 6, OpAdd (CbConst 176 1 5) (CbConst 176 1 6), OpUnsetReturn 4, OpUnsetVar 5, OpUnsetVar 6, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested addition and subtraction" $
      let stmt = FuncDeclStmt "n" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Add (BinArith Subtract (Var "a") (LitInt 2)) (Var "b")))))
          compile = Compile [] [] (Scope ["a", "b"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpSub (CbConst 176 1 0) (CbConst 160 1 2), OpUnsetReturn 5, OpAdd (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested multiplication and addition" $
      let stmt = FuncDeclStmt "o" [("x", TInt, Nothing), ("y", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Multiply (BinArith Add (Var "x") (LitInt 3)) (Var "y")))))
          compile = Compile [] [] (Scope ["x", "y"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpAdd (CbConst 176 1 0) (CbConst 160 1 3), OpUnsetReturn 5, OpMul (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested subtraction and multiplication" $
      let stmt = FuncDeclStmt "p" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Subtract (BinArith Multiply (Var "a") (LitInt 2)) (Var "b")))))
          compile = Compile [] [] (Scope ["a", "b"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpMul (CbConst 176 1 0) (CbConst 160 1 2), OpUnsetReturn 5, OpSub (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested addition and modulo" $
      let stmt = FuncDeclStmt "q" [("x", TInt, Nothing), ("y", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Add (BinArith Modulo (Var "x") (LitInt 3)) (Var "y")))))
          compile = Compile [] [] (Scope ["x", "y"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpMod (CbConst 176 1 0) (CbConst 160 1 3), OpUnsetReturn 5, OpAdd (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested subtraction and addition" $
      let stmt = FuncDeclStmt "r" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Subtract (BinArith Add (Var "a") (LitInt 3)) (Var "b")))))
          compile = Compile [] [] (Scope ["a", "b"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpAdd (CbConst 176 1 0) (CbConst 160 1 3), OpUnsetReturn 5, OpSub (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested multiplication and subtraction" $
      let stmt = FuncDeclStmt "s" [("x", TInt, Nothing), ("y", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Multiply (BinArith Subtract (Var "x") (LitInt 3)) (Var "y")))))
          compile = Compile [] [] (Scope ["x", "y"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpSub (CbConst 176 1 0) (CbConst 160 1 3), OpUnsetReturn 5, OpMul (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected

    it "compiles a function with nested modulo and addition" $
      let stmt = FuncDeclStmt "t" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt (Just (ReturnStmt (Just (BinArith Modulo (BinArith Add (Var "a") (LitInt 3)) (Var "b")))))
          compile = Compile [] [] (Scope ["a", "b"] "" 0)
          expected = [OpCreateVar 1 0, OpCreateVar 1 0, OpUnsetArg 3 0, OpUnsetArg 4 1, OpCreateVar 1 0, OpCreateVar 1 0, OpAdd (CbConst 176 1 0) (CbConst 160 1 3), OpUnsetReturn 5, OpMod (CbConst 176 1 5) (CbConst 176 1 1), OpUnsetReturn 4, OpUnsetVar 5, OpSetTmp 1 (CbConst 176 1 4), OpSetReturn 1 (CbConst 176 1 4294967295), OpReturn]
      in compileFunction stmt compile `shouldBe` expected
module FunctionSpec (spec) where

import Test.Hspec
import Parser.File (parseFromString)
import Parser.Stmt (Stmt(..))
import Parser.AST (Expr(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..), RangeType(..))
import Parser.Type (Type(..))
import Parser.Pattern (Pattern(..))

spec :: Spec
spec = do
  describe "Function parsing" $ do

    -- Fonctions déjà existantes

    it "parses a function with logical AND in the body" $ do
      let input = "fn myFunction(x: bool, y: bool) -> bool { return x && y; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TBool, Nothing), ("y", TBool, Nothing)]
            TBool
            (Just (BlockStmt [ReturnStmt (Just (BinLogic And (Var "x") (Var "y")))])) ]

    it "parses a function with a negation operator" $ do
      let input = "fn myFunction(x: bool) -> bool { return !x; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TBool, Nothing)]
            TBool
            (Just (BlockStmt [ReturnStmt (Just (UnaryLogic Not (Var "x")))])) ]

    it "parses a function with a while loop" $ do
      let input = "fn loopFunction() -> void { while (true) { return; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "loopFunction"
            []
            TVoid
            (Just (BlockStmt
              [ WhileStmt (LitBool True) [ReturnStmt Nothing] ])) ]

    it "parses a function with a for loop with range" $ do
      let input = "fn myFunction() -> void { for (i in 0..10) { return; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            TVoid
            (Just (BlockStmt
              [ ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive Nothing [ReturnStmt Nothing] ])) ]

    it "parses a function with a for loop with step" $ do
      let input = "fn myFunction() -> void { for (i in 0..10 step 2) { return; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            TVoid
            (Just (BlockStmt
              [ ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive (Just (LitInt 2)) [ReturnStmt Nothing] ])) ]

    it "parses a function with an arithmetic expression in return" $ do
      let input = "fn add(x: int, y: int) -> int { return x + y * 2; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "add"
            [("x", TInt, Nothing), ("y", TInt, Nothing)]
            TInt
            (Just (BlockStmt
              [ ReturnStmt (Just (BinArith Add (Var "x") (BinArith Multiply (Var "y") (LitInt 2)))) ])) ]

    it "parses a function with a conditional return" $ do
      let input = "fn check(x: int) -> bool { return x > 10 ? true : false; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "check"
            [("x", TInt, Nothing)]
            TBool
            (Just (BlockStmt
              [ ReturnStmt (Just (Ternary (BinComp GreaterThan (Var "x") (LitInt 10)) (LitBool True) (LitBool False)))])) ]

    it "parses a function with nested blocks" $ do
      let input = "fn myFunction() -> void { { let x = 42; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            TVoid
            (Just (BlockStmt
              [ BlockStmt [LetStmt "x" (Just TInt) (LitInt 42)] ])) ]

    it "parses a function with a break statement in a loop" $ do
      let input = "fn loopFunction() -> void { for (i in 0..10) { if (i == 5) { break; } } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "loopFunction"
            []
            TVoid
            (Just (BlockStmt
              [ ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive Nothing
                  [ IfStmt (BinComp Equal (Var "i") (LitInt 5)) (BlockStmt [BreakStmt]) Nothing ]
              ])) ]

    it "parses a function with a continue statement in a loop" $ do
      let input = "fn loopFunction() -> void { for (i in 0..10) { if (i == 5) { continue; } } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "loopFunction"
            []
            TVoid
            (Just (BlockStmt
              [ ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive Nothing
                  [ IfStmt (BinComp Equal (Var "i") (LitInt 5)) (BlockStmt [ContinueStmt]) Nothing ]
              ])) ]

    it "parses a function with nested if-else statements" $ do
      let input = "fn myFunction(x: int) -> void { if (x > 0) { if (x == 1) { return; } } else { return; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TInt, Nothing)]
            TVoid
            (Just (BlockStmt
              [ IfStmt (BinComp GreaterThan (Var "x") (LitInt 0))
                  (BlockStmt
                    [ IfStmt (BinComp Equal (Var "x") (LitInt 1)) (BlockStmt [ReturnStmt Nothing]) Nothing ])
                  (Just (BlockStmt [ReturnStmt Nothing]))
              ])) ]

    it "parses a function with a long parameter list" $ do
          let input = "fn myFunction(a: int, b: int, c: int, d: int, e: int) -> void {}"
          parseFromString input `shouldBe` Right
            [ FuncDeclStmt "myFunction" 
                [("a", TInt, Nothing), ("b", TInt, Nothing), ("c", TInt, Nothing), ("d", TInt, Nothing), ("e", TInt, Nothing)]
                TVoid 
                (Just (BlockStmt [])) ]

    it "parses a function with deeply nested expressions" $ do
      let input = "fn myFunction() -> int { return (1 + (2 * (3 - (4 / 5)))); }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            TInt
            (Just (BlockStmt [ReturnStmt (Just (BinArith Add 
                (LitInt 1) 
                (BinArith Multiply 
                    (LitInt 2) 
                    (BinArith Subtract 
                        (LitInt 3) 
                        (BinArith Divide (LitInt 4) (LitInt 5))))))])) ]

    it "parses a function with a tuple return type" $ do
      let input = "fn myFunction() -> (int, bool) { return (42, true); }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            (TTuple [TInt, TBool])
            (Just (BlockStmt [ReturnStmt (Just (LitTuple [LitInt 42, LitBool True]))])) ]

    it "parses a function with a nested ternary operator" $ do
      let input = "fn myFunction(x: int) -> bool { return x > 10 ? (x < 20 ? true : false) : false; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TInt, Nothing)]
            TBool
            (Just (BlockStmt [ReturnStmt (Just 
              (Ternary 
                (BinComp GreaterThan (Var "x") (LitInt 10)) 
                (Ternary 
                  (BinComp LessThan (Var "x") (LitInt 20)) 
                  (LitBool True) 
                  (LitBool False)) 
                (LitBool False)))])) ]

    it "parses a function with multiple return statements" $ do
      let input = "fn myFunction(x: int) -> int { if (x > 10) { return x; } else { return 0; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TInt, Nothing)]
            TInt
            (Just (BlockStmt 
              [ IfStmt 
                  (BinComp GreaterThan (Var "x") (LitInt 10)) 
                  (BlockStmt [ReturnStmt (Just (Var "x"))]) 
                  (Just (BlockStmt [ReturnStmt (Just (LitInt 0))])) ])) ]

    it "parses a function with multiple nested blocks" $ do
      let input = "fn myFunction() -> void { { { return; } } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            TVoid
            (Just (BlockStmt [BlockStmt [BlockStmt [ReturnStmt Nothing]]])) ]

    it "parses a function with boolean expressions" $ do
      let input = "fn myFunction(a: bool, b: bool) -> bool { return a && !b; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("a", TBool, Nothing), ("b", TBool, Nothing)]
            TBool
            (Just (BlockStmt [ReturnStmt (Just (BinLogic And (Var "a") (UnaryLogic Not (Var "b"))))])) ]

    it "parses a function with a match expression" $ do
      let input = "fn myFunction(x: int) -> string { match x { 1 => \"one\", 2 => \"two\", _ => \"other\" } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TInt, Nothing)]
            TString
            (Just (BlockStmt 
              [ MatchStmt (Var "x") 
                  [ (PatLitInt 1, ExprStmt (LitString "one"))
                  , (PatLitInt 2, ExprStmt (LitString "two"))
                  , (PatWildcard, ExprStmt (LitString "other"))
                  ]])) ]

    it "parses a function with array indexing" $ do
      let input = "fn myFunction(arr: int[]) -> int { return arr[0]; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("arr", TArray TInt, Nothing)]
            TInt
            (Just (BlockStmt [ReturnStmt (Just (ArrayIndex (Var "arr") (LitInt 0)))])) ]

    it "parses a function with array assignment" $ do
      let input = "fn myFunction(arr: int[]) -> void { arr[0] = 42; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("arr", TArray TInt, Nothing)]
            TVoid
            (Just (BlockStmt [ExprStmt (Assign (ArrayIndex (Var "arr") (LitInt 0)) (LitInt 42))])) ]

    it "parses a function with string concatenation" $ do
      let input = "fn myFunction() -> string { return \"Hello\" + \" \" + \"World\"; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            []
            TString
            (Just (BlockStmt [ReturnStmt (Just (BinArith Add 
              (BinArith Add (LitString "Hello") (LitString " ")) 
              (LitString "World")))])) ]
              
    it "parses a function with an array of functions as parameter" $ do
      let input = "fn applyFunctions(fns: ((int) -> int)[], x: int) -> int { return fns[0](x); }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "applyFunctions"
            [("fns", TArray (TFunc [TInt] TInt), Nothing), ("x", TInt, Nothing)]
            TInt
            (Just (BlockStmt 
              [ ReturnStmt 
                  (Just 
                    (FuncCall 
                      (ArrayIndex (Var "fns") (LitInt 0))  -- Accessing the function from the array
                      [Var "x"]                            -- Applying it to the argument 'x'
                    )
                  )
              ])
            )
        ]

    it "parses a function with nested ternary expressions" $ do
      let input = "fn myFunction(x: int, y: int) -> bool { return x > 10 ? y < 5 ? true : false : false; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TInt, Nothing), ("y", TInt, Nothing)]
            TBool
            (Just (BlockStmt [ReturnStmt (Just 
              (Ternary 
                (BinComp GreaterThan (Var "x") (LitInt 10)) 
                (Ternary 
                  (BinComp LessThan (Var "y") (LitInt 5)) 
                  (LitBool True) 
                  (LitBool False)) 
                (LitBool False)))])) ]

    it "parses a function with a switch-like match expression" $ do
      let input = "fn myFunction(x: int) -> string { match x { 1 => \"one\", 2 => \"two\", _ => \"other\" } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("x", TInt, Nothing)]
            TString
            (Just (BlockStmt 
              [ MatchStmt (Var "x") 
                  [ (PatLitInt 1, ExprStmt (LitString "one"))
                  , (PatLitInt 2, ExprStmt (LitString "two"))
                  , (PatWildcard, ExprStmt (LitString "other"))
                  ]])) ]

    it "parses a function with multiple for loops" $ do
      let input = "fn multiLoop() -> void { for (i in 0..5) { for (j in 0..3) { return; } } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "multiLoop"
            []
            TVoid
            (Just (BlockStmt
              [ ForRangeStmt "i" (LitInt 0) (LitInt 5) Exclusive Nothing
                  [ ForRangeStmt "j" (LitInt 0) (LitInt 3) Exclusive Nothing [ReturnStmt Nothing] ]
              ])) ]

    it "parses a function with multiple array accesses" $ do
      let input = "fn accessArray(arr: int[][]) -> int { return arr[0][1]; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "accessArray"
            [("arr", TArray (TArray TInt), Nothing)]
            TInt
            (Just (BlockStmt [ReturnStmt (Just (ArrayIndex (ArrayIndex (Var "arr") (LitInt 0)) (LitInt 1)))])) ]

    it "parses a function with logical OR" $ do
      let input = "fn myFunction(a: bool, b: bool) -> bool { return a || b; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "myFunction"
            [("a", TBool, Nothing), ("b", TBool, Nothing)]
            TBool
            (Just (BlockStmt [ReturnStmt (Just (BinLogic Or (Var "a") (Var "b")))])) ]

    it "parses a function with nested if-else and for loop" $ do
      let input = "fn complexFunction(x: int) -> void { if (x > 0) { for (i in 0..x) { return; } } else { return; } }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "complexFunction"
            [("x", TInt, Nothing)]
            TVoid
            (Just (BlockStmt
              [ IfStmt
                  (BinComp GreaterThan (Var "x") (LitInt 0))
                  (BlockStmt [ForRangeStmt "i" (LitInt 0) (Var "x") Exclusive Nothing [ReturnStmt Nothing]])
                  (Just (BlockStmt [ReturnStmt Nothing]))
              ])) ]

    it "parses a function with complex logic" $ do
      let input = "fn complexFunction(arr: int[][]) -> int { for (i in 0..10) { if (arr[i][0] > 0) { return arr[i][1]; } } return -1; }"
      parseFromString input `shouldBe` Right
        [ FuncDeclStmt
            "complexFunction"
            [("arr", TArray (TArray TInt), Nothing)]
            TInt
            (Just (BlockStmt
              [ ForRangeStmt "i" (LitInt 0) (LitInt 10) Exclusive Nothing
                  [ IfStmt
                      (BinComp GreaterThan (ArrayIndex (ArrayIndex (Var "arr") (Var "i")) (LitInt 0)) (LitInt 0))
                      (BlockStmt [ReturnStmt (Just (ArrayIndex (ArrayIndex (Var "arr") (Var "i")) (LitInt 1)))])
                      Nothing
                  ]
              , ReturnStmt (Just (LitInt (-1)))
              ])) ]
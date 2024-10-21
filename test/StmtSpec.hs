module StmtSpec (spec) where

import Test.Hspec
import Parser.Stmt (parseStmt, Stmt(..), RangeType(..))
import Parser.Expr (Expr(..), ArithOp(..), CompOp(..), LogicOp(..))
import Parser.Type (Type(..))

spec :: Spec
spec = do
  describe "Statement parsing" $ do

    -- Tests pour let declaration
    it "parses let declaration with type" $ do
      let input = "let x: int = 42;"
      parseStmt input `shouldBe` Right (LetStmt "x" (Just TInt) (LitInt 42))

    it "parses let declaration with expression" $ do
      let input = "let y = 10 + 20;"
      parseStmt input `shouldBe` Right (LetStmt "y" Nothing (BinArith Add (LitInt 10) (LitInt 20)))

    -- Tests pour return statement
    it "parses return with expression" $ do
      let input = "return x + 1;"
      parseStmt input `shouldBe` Right (ReturnStmt (Just (BinArith Add (Var "x") (LitInt 1))))

    it "parses return without expression" $ do
      let input = "return;"
      parseStmt input `shouldBe` Right (ReturnStmt Nothing)

    -- Tests pour bloc d'instructions
    it "parses empty block" $ do
      let input = "{}"
      parseStmt input `shouldBe` Right (BlockStmt [])

    it "parses block with multiple statements" $ do
      let input = "{ let x = 1; let y = 2; return x + y; }"
      parseStmt input `shouldBe` Right (BlockStmt
        [ LetStmt "x" Nothing (LitInt 1)
        , LetStmt "y" Nothing (LitInt 2)
        , ReturnStmt (Just (BinArith Add (Var "x") (Var "y")))
        ])

    -- Tests pour if statement
    it "parses if statement without else branch" $ do
      let input = "if (x > 5) { return 42; }"
      parseStmt input `shouldBe` Right (IfStmt (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (BlockStmt [ReturnStmt (Just (LitInt 42))])  
                                                Nothing)

    it "parses if statement with else if and else branch" $ do
      let input = "if (x > 5) { return 42; } else if (x == 5) { return 0; } else { return -1; }"
      parseStmt input `shouldBe` Right (IfStmt (BinComp GreaterThan (Var "x") (LitInt 5))
                                               (BlockStmt [ReturnStmt (Just (LitInt 42))])
                                               (Just (IfStmt (BinComp Equal (Var "x") (LitInt 5))
                                                             (BlockStmt [ReturnStmt (Just (LitInt 0))])
                                                             (Just (BlockStmt [ReturnStmt (Just (LitInt (-1)))])))))

    -- Tests pour while statement
    it "parses while statement with complex condition" $ do
      let input = "while (x > 0 && y < 10) { x = x - 1; }"
      parseStmt input `shouldBe` Right (WhileStmt (BinLogic And
                                                  (BinComp GreaterThan (Var "x") (LitInt 0))
                                                  (BinComp LessThan (Var "y") (LitInt 10)))
                                              [ExprStmt (Assign (Var "x") (BinArith Subtract (Var "x") (LitInt 1)))])

    -- Tests pour do-while statement
    it "parses do-while loop" $ do
      let input = "do { x = x + 1; } while (x < 10);"
      parseStmt input `shouldBe` Right (DoWhileStmt
                                        [ExprStmt (Assign (Var "x") (BinArith Add (Var "x") (LitInt 1)))]
                                        (BinComp LessThan (Var "x") (LitInt 10)))

    -- Tests pour for loops
    it "parses for loop with range (exclusive)" $ do
      let input = "for i in 1..10 { let i = i + 1; }"
      parseStmt input `shouldBe` Right (ForRangeStmt "i" (LitInt 1) (LitInt 10) Exclusive Nothing
                                        [LetStmt "i" Nothing (BinArith Add (Var "i") (LitInt 1))])

    it "parses for loop with range (inclusive)" $ do
      let input = "for i in 1..=10 { let i = i + 1; }"
      parseStmt input `shouldBe` Right (ForRangeStmt "i" (LitInt 1) (LitInt 10) Inclusive Nothing
                                        [LetStmt "i" Nothing (BinArith Add (Var "i") (LitInt 1))])

    -- Tests pour break et continue
    it "parses break statement" $ do
      let input = "break;"
      parseStmt input `shouldBe` Right BreakStmt

    it "parses continue statement" $ do
      let input = "continue;"
      parseStmt input `shouldBe` Right ContinueStmt

    -- Tests pour for classique
    it "parses for loop with initialization, condition and increment" $ do
      let input = "for (let i = 0; i < 10; i = i + 1) { let x = 0; }"
      parseStmt input `shouldBe` Right (ForClassicStmt
                                        (Just (LetStmt "i" Nothing (LitInt 0)))  
                                        (BinComp LessThan (Var "i") (LitInt 10)) 
                                        (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1))))  
                                        [LetStmt "x" Nothing (LitInt 0)])

    it "parses for loop without initialization" $ do
      let input = "for (; i < 10; i = i + 1) { let x = 0; }"
      parseStmt input `shouldBe` Right (ForClassicStmt
                                        Nothing
                                        (BinComp LessThan (Var "i") (LitInt 10))
                                        (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1))))
                                        [LetStmt "x" Nothing (LitInt 0)])

    it "parses for loop without condition" $ do
      let input = "for (let i = 0;; i = i + 1) { let x = 0; }"
      parseStmt input `shouldBe` Right (ForClassicStmt
                                        (Just (LetStmt "i" Nothing (LitInt 0)))
                                        (LitBool True)  
                                        (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1))))
                                        [LetStmt "x" Nothing (LitInt 0)])

    it "parses for loop without increment" $ do
      let input = "for (let i = 0; i < 10;) { let x = 0; }"
      parseStmt input `shouldBe` Right (ForClassicStmt
                                        (Just (LetStmt "i" Nothing (LitInt 0)))
                                        (BinComp LessThan (Var "i") (LitInt 10))
                                        Nothing
                                        [LetStmt "x" Nothing (LitInt 0)])

    -- Test sur l'incrémentation complexe
    it "parses for loop with complex increment" $ do
      let input = "for (let i = 0; i < 10; i = i * 2 + 1) { let x = 0; }"
      parseStmt input `shouldBe` Right (ForClassicStmt
                                        (Just (LetStmt "i" Nothing (LitInt 0)))
                                        (BinComp LessThan (Var "i") (LitInt 10))
                                        (Just (Assign (Var "i") (BinArith Add (BinArith Multiply (Var "i") (LitInt 2)) (LitInt 1))))
                                        [LetStmt "x" Nothing (LitInt 0)])

    -- Ajout de tests pour la déclaration, return, et for avec des scénarios variés

    -- Ajout de cas de for sans condition ni incrémentation
    it "parses for loop with only initialization" $ do
      let input = "for (let i = 0;;) { let x = 0; }"
      parseStmt input `shouldBe` Right (ForClassicStmt
                                        (Just (LetStmt "i" Nothing (LitInt 0)))
                                        (LitBool True)  
                                        Nothing
                                        [LetStmt "x" Nothing (LitInt 0)])

    it "parses if-else statement with block statements" $ do
      let input = "if (x > 10) { let a = 1; } else { let b = 2; }"
      parseStmt input `shouldBe` Right (IfStmt (BinComp GreaterThan (Var "x") (LitInt 10))
                                               (BlockStmt [LetStmt "a" Nothing (LitInt 1)])
                                               (Just (BlockStmt [LetStmt "b" Nothing (LitInt 2)])))

    -- Complexe if-else avec else-if
    it "parses if-else if-else statement" $ do
      let input = "if (x > 10) { let a = 1; } else if (x == 5) { let b = 2; } else { let c = 3; }"
      parseStmt input `shouldBe` Right (IfStmt (BinComp GreaterThan (Var "x") (LitInt 10))
                                               (BlockStmt [LetStmt "a" Nothing (LitInt 1)])
                                               (Just (IfStmt (BinComp Equal (Var "x") (LitInt 5))
                                                             (BlockStmt [LetStmt "b" Nothing (LitInt 2)])
                                                             (Just (BlockStmt [LetStmt "c" Nothing (LitInt 3)])))))

    it "parses complex do-while loop with multiple statements" $ do
      let input = "do { let a = 1; let b = a + 2; } while (a < 10);"
      parseStmt input `shouldBe` Right (DoWhileStmt
                                        [ LetStmt "a" Nothing (LitInt 1)
                                        , LetStmt "b" Nothing (BinArith Add (Var "a") (LitInt 2))
                                        ]
                                        (BinComp LessThan (Var "a") (LitInt 10)))

    it "parses let declaration with array type" $ do
      let input = "let arr: int[] = [1, 2, 3];"
      parseStmt input `shouldBe` Right (LetStmt "arr" (Just (TArray TInt)) (LitArray [LitInt 1, LitInt 2, LitInt 3]))

    it "parses let declaration with tuple type" $ do
      let input = "let tuple: (int, string) = (42, \"hello\");"
      parseStmt input `shouldBe` Right (LetStmt "tuple" (Just (TTuple [TInt, TString])) (LitTuple [LitInt 42, LitString "hello"]))

    -- Tests supplémentaires pour return statements
    it "parses return with complex expression" $ do
      let input = "return (x + 1) * (y - 2);"
      parseStmt input `shouldBe` Right (ReturnStmt (Just (BinArith Multiply (BinArith Add (Var "x") (LitInt 1)) (BinArith Subtract (Var "y") (LitInt 2)))))

    it "parses return with function call" $ do
      let input = "return foo(42, x + y);"
      parseStmt input `shouldBe` Right (ReturnStmt (Just (FuncCall "foo" [LitInt 42, BinArith Add (Var "x") (Var "y")])))

    -- Tests supplémentaires pour if statements
    it "parses if statement with logical operations" $ do
      let input = "if (x > 5 && y < 10) { return 42; }"
      parseStmt input `shouldBe` Right (IfStmt (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 5)) (BinComp LessThan (Var "y") (LitInt 10))) (BlockStmt [ReturnStmt (Just (LitInt 42))]) Nothing)

    it "parses if statement with nested conditions" $ do
      let input = "if (x > 5) { if (y < 10) { return 42; } else { return 0; } }"
      parseStmt input `shouldBe` Right (IfStmt (BinComp GreaterThan (Var "x") (LitInt 5)) (BlockStmt [IfStmt (BinComp LessThan (Var "y") (LitInt 10)) (BlockStmt [ReturnStmt (Just (LitInt 42))]) (Just (BlockStmt [ReturnStmt (Just (LitInt 0))]))]) Nothing)

    -- Tests supplémentaires pour while loops
    it "parses while loop with logical operations" $ do
      let input = "while (x > 0 && y < 10) { x = x - 1; y = y + 1; }"
      parseStmt input `shouldBe` Right (WhileStmt (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 0)) (BinComp LessThan (Var "y") (LitInt 10))) [ExprStmt (Assign (Var "x") (BinArith Subtract (Var "x") (LitInt 1))), ExprStmt (Assign (Var "y") (BinArith Add (Var "y") (LitInt 1)))])

    it "parses while loop with break" $ do
      let input = "while (x > 0) { if (x == 1) { break; } x = x - 1; }"
      parseStmt input `shouldBe` Right (WhileStmt (BinComp GreaterThan (Var "x") (LitInt 0)) [IfStmt (BinComp Equal (Var "x") (LitInt 1)) (BlockStmt [BreakStmt]) Nothing, ExprStmt (Assign (Var "x") (BinArith Subtract (Var "x") (LitInt 1)))])

    -- Tests supplémentaires pour do-while loops
    it "parses do-while loop with logical operations" $ do
      let input = "do { x = x - 1; y = y + 1; } while (x > 0 && y < 10);"
      parseStmt input `shouldBe` Right (DoWhileStmt [ExprStmt (Assign (Var "x") (BinArith Subtract (Var "x") (LitInt 1))), ExprStmt (Assign (Var "y") (BinArith Add (Var "y") (LitInt 1)))] (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 0)) (BinComp LessThan (Var "y") (LitInt 10))))

    -- Tests supplémentaires pour for loops (range)
    it "parses for loop with step and complex body" $ do
      let input = "for i in 1..10 step 2 { let x = i * 2; let y = x + 1; }"
      parseStmt input `shouldBe` Right (ForRangeStmt "i" (LitInt 1) (LitInt 10) Exclusive (Just (LitInt 2)) [LetStmt "x" Nothing (BinArith Multiply (Var "i") (LitInt 2)), LetStmt "y" Nothing (BinArith Add (Var "x") (LitInt 1))])

    it "parses for loop with step and nested loops" $ do
      let input = "for i in 1..10 step 2 { for j in 0..i { let x = i + j; } }"
      parseStmt input `shouldBe` Right (ForRangeStmt "i" (LitInt 1) (LitInt 10) Exclusive (Just (LitInt 2)) [ForRangeStmt "j" (LitInt 0) (Var "i") Exclusive Nothing [LetStmt "x" Nothing (BinArith Add (Var "i") (Var "j"))]])

    -- Tests supplémentaires pour for loops (classique)
    it "parses classic for loop with multiple statements in body" $ do
      let input = "for (let i = 0; i < 10; i = i + 1) { let x = 0; x = i * 2; }"
      parseStmt input `shouldBe` Right (ForClassicStmt (Just (LetStmt "i" Nothing (LitInt 0))) (BinComp LessThan (Var "i") (LitInt 10)) (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1)))) [LetStmt "x" Nothing (LitInt 0), ExprStmt (Assign (Var "x") (BinArith Multiply (Var "i") (LitInt 2)))])

    it "parses classic for loop with complex condition" $ do
      let input = "for (let i = 0; i < 10 && j < 5; i = i + 1) { let x = i + j; }"
      parseStmt input `shouldBe` Right (ForClassicStmt (Just (LetStmt "i" Nothing (LitInt 0))) (BinLogic And (BinComp LessThan (Var "i") (LitInt 10)) (BinComp LessThan (Var "j") (LitInt 5))) (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1)))) [LetStmt "x" Nothing (BinArith Add (Var "i") (Var "j"))])

    -- Tests pour break et continue dans les boucles for
    it "parses for loop with break and continue" $ do
      let input = "for (let i = 0; i < 10; i = i + 1) { if (i == 5) { break; } if (i == 3) { continue; } }"
      parseStmt input `shouldBe` Right (ForClassicStmt (Just (LetStmt "i" Nothing (LitInt 0))) (BinComp LessThan (Var "i") (LitInt 10)) (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1)))) [IfStmt (BinComp Equal (Var "i") (LitInt 5)) (BlockStmt [BreakStmt]) Nothing, IfStmt (BinComp Equal (Var "i") (LitInt 3)) (BlockStmt [ContinueStmt]) Nothing])

    -- Tests supplémentaires pour les instructions block
    it "parses block statement with multiple let and return" $ do
      let input = "{ let x = 0; let y = 1; return x + y; }"
      parseStmt input `shouldBe` Right (BlockStmt [LetStmt "x" Nothing (LitInt 0), LetStmt "y" Nothing (LitInt 1), ReturnStmt (Just (BinArith Add (Var "x") (Var "y")))])

    it "parses block statement with nested if" $ do
      let input = "{ let x = 0; if (x > 5) { return 42; } }"
      parseStmt input `shouldBe` Right (BlockStmt [LetStmt "x" Nothing (LitInt 0), IfStmt (BinComp GreaterThan (Var "x") (LitInt 5)) (BlockStmt [ReturnStmt (Just (LitInt 42))]) Nothing])

    -- Tests pour do-while avec multiple instructions dans le bloc
    it "parses do-while loop with complex body" $ do
      let input = "do { let x = 0; let y = x + 1; } while (x < 10);"
      parseStmt input `shouldBe` Right (DoWhileStmt [LetStmt "x" Nothing (LitInt 0), LetStmt "y" Nothing (BinArith Add (Var "x") (LitInt 1))] (BinComp LessThan (Var "x") (LitInt 10)))

    -- Tests supplémentaires pour les affectations
    it "parses assignment with array index" $ do
      let input = "arr[0] = 42;"
      parseStmt input `shouldBe` Right (ExprStmt (Assign (ArrayIndex (Var "arr") (LitInt 0)) (LitInt 42)))


    it "parses assignment with tuple access" $ do
      let input = "tuple._1 = 42;"
      parseStmt input `shouldBe` Right (ExprStmt (Assign (FuncCall "tuple" [LitString "_1"]) (LitInt 42)))

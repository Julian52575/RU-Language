module StmtSpec (spec) where

import Test.Hspec
import Parser.Stmt (parseStmt, Stmt(..), RangeType(..))
import Parser.Expr (Expr(..), ArithOp(..), CompOp(..), LogicOp(..))
import Parser.Type (Type(..))
import Parser.Pattern (Pattern(..))

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
      parseStmt input `shouldBe` Right (ReturnStmt (Just (FuncCall (Var "foo") [LitInt 42, BinArith Add (Var "x") (Var "y")])))

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


        -- Tests pour match statement
    it "parses match statement with literal patterns" $ do
      let input = "match x { 1 => return 42, 2 => return 43 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x") 
                                          [ (PatLitInt 1, ReturnStmt (Just (LitInt 42)))
                                          , (PatLitInt 2, ReturnStmt (Just (LitInt 43)))
                                          ])

    it "parses match statement with boolean patterns" $ do
      let input = "match x { true => return 1, false => return 0 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x") 
                                          [ (PatLitBool True, ReturnStmt (Just (LitInt 1)))
                                          , (PatLitBool False, ReturnStmt (Just (LitInt 0)))
                                          ])

    it "parses match statement with wildcard pattern" $ do
      let input = "match x { _ => return 100 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x") 
                                          [ (PatWildcard, ReturnStmt (Just (LitInt 100))) ])

    it "parses match statement with range patterns" $ do
      let input = "match x { 1..10 => return 42, 11..=20 => return 43 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                          [ (PatRange (PatLitInt 1) (PatLitInt 10), ReturnStmt (Just (LitInt 42)))
                                          , (PatRangeInclusive (PatLitInt 11) (PatLitInt 20), ReturnStmt (Just (LitInt 43)))
                                          ])

    it "parses match statement with tuple patterns" $ do
      let input = "match x { (1, 2) => return 42, (3, 4) => return 43 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                          [ (PatTuple [PatLitInt 1, PatLitInt 2], ReturnStmt (Just (LitInt 42)))
                                          , (PatTuple [PatLitInt 3, PatLitInt 4], ReturnStmt (Just (LitInt 43)))
                                          ])

    it "parses match statement with array patterns" $ do
      let input = "match x { [1, 2, 3] => return 42, [4, 5, 6] => return 43 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                          [ (PatArray [PatLitInt 1, PatLitInt 2, PatLitInt 3], ReturnStmt (Just (LitInt 42)))
                                          , (PatArray [PatLitInt 4, PatLitInt 5, PatLitInt 6], ReturnStmt (Just (LitInt 43)))
                                          ])

    it "parses match statement with or patterns" $ do
      let input = "match x { 1 | 2 | 3 => return 42, 4 | 5 | 6 => return 43 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                          [ (PatOr [PatLitInt 1, PatLitInt 2, PatLitInt 3], ReturnStmt (Just (LitInt 42)))
                                          , (PatOr [PatLitInt 4, PatLitInt 5, PatLitInt 6], ReturnStmt (Just (LitInt 43)))
                                          ])

    it "parses match statement with variable patterns" $ do
      let input = "match x { y => return y + 1 }"
      parseStmt input `shouldBe` Right (MatchStmt (Var "x") 
                                          [ (PatVar "y", ReturnStmt (Just (BinArith Add (Var "y") (LitInt 1)))) ])

-- Tests pour match avec des patterns littéraux
  it "parses match statement with integer literal patterns" $ do
    let input = "match x { 1 => return 42, 2 => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (LitInt 42)))
                                        , (PatLitInt 2, ReturnStmt (Just (LitInt 43))) ])

  it "parses match statement with string literal patterns" $ do
    let input = "match x { \"a\" => return 42, \"b\" => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitString "a", ReturnStmt (Just (LitInt 42)))
                                        , (PatLitString "b", ReturnStmt (Just (LitInt 43))) ])

  it "parses match statement with boolean literal patterns" $ do
    let input = "match x { true => return 1, false => return 0 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitBool True, ReturnStmt (Just (LitInt 1)))
                                        , (PatLitBool False, ReturnStmt (Just (LitInt 0))) ])

  -- Tests pour match avec des blocs
  it "parses match statement with block patterns" $ do
    let input = "match x { 1 => { let a = 1; return a; }, 2 => { let b = 2; return b; } }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, BlockStmt [LetStmt "a" Nothing (LitInt 1), ReturnStmt (Just (Var "a"))])
                                        , (PatLitInt 2, BlockStmt [LetStmt "b" Nothing (LitInt 2), ReturnStmt (Just (Var "b"))]) ])

  -- Tests pour match avec des plages exclusives et inclusives
  it "parses match statement with exclusive range patterns" $ do
    let input = "match x { 1..10 => return 42, 11..20 => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatRange (PatLitInt 1) (PatLitInt 10), ReturnStmt (Just (LitInt 42)))
                                        , (PatRange (PatLitInt 11) (PatLitInt 20), ReturnStmt (Just (LitInt 43))) ])

  it "parses match statement with inclusive range patterns" $ do
    let input = "match x { 1..=10 => return 42, 11..=20 => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatRangeInclusive (PatLitInt 1) (PatLitInt 10), ReturnStmt (Just (LitInt 42)))
                                        , (PatRangeInclusive (PatLitInt 11) (PatLitInt 20), ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des motifs or
  it "parses match statement with or patterns" $ do
    let input = "match x { 1 | 2 | 3 => return 42, 4 | 5 | 6 => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatOr [PatLitInt 1, PatLitInt 2, PatLitInt 3], ReturnStmt (Just (LitInt 42)))
                                        , (PatOr [PatLitInt 4, PatLitInt 5, PatLitInt 6], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des tuples
  it "parses match statement with tuple patterns" $ do
    let input = "match x { (1, 2) => return 42, (3, 4) => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatTuple [PatLitInt 1, PatLitInt 2], ReturnStmt (Just (LitInt 42)))
                                        , (PatTuple [PatLitInt 3, PatLitInt 4], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des tableaux
  it "parses match statement with array patterns" $ do
    let input = "match x { [1, 2, 3] => return 42, [4, 5, 6] => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatArray [PatLitInt 1, PatLitInt 2, PatLitInt 3], ReturnStmt (Just (LitInt 42)))
                                        , (PatArray [PatLitInt 4, PatLitInt 5, PatLitInt 6], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des variables
  it "parses match statement with variable patterns" $ do
    let input = "match x { y => return y + 1 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatVar "y", ReturnStmt (Just (BinArith Add (Var "y") (LitInt 1)))) ])

  -- Tests pour match avec le wildcard
  it "parses match statement with wildcard pattern" $ do
    let input = "match x { _ => return 100 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatWildcard, ReturnStmt (Just (LitInt 100))) ])

  -- Tests pour match avec des combinaisons de motifs
  it "parses match statement with combined patterns" $ do
    let input = "match x { (1 | 2, 3..5) => return 42, (3, 4 | 5) => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatTuple [PatOr [PatLitInt 1, PatLitInt 2], PatRange (PatLitInt 3) (PatLitInt 5)], ReturnStmt (Just (LitInt 42)))
                                        , (PatTuple [PatLitInt 3, PatOr [PatLitInt 4, PatLitInt 5]], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des plages et des tuples
  it "parses match statement with range and tuple patterns" $ do
    let input = "match x { (1..5, 2..3) => return 42, (6..10, 4..6) => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatTuple [PatRange (PatLitInt 1) (PatLitInt 5), PatRange (PatLitInt 2) (PatLitInt 3)], ReturnStmt (Just (LitInt 42)))
                                        , (PatTuple [PatRange (PatLitInt 6) (PatLitInt 10), PatRange (PatLitInt 4) (PatLitInt 6)], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des combinaisons tableau et or
  it "parses match statement with array and or patterns" $ do
    let input = "match x { [1 | 2, 3 | 4] => return 42, [5 | 6, 7 | 8] => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatArray [PatOr [PatLitInt 1, PatLitInt 2], PatOr [PatLitInt 3, PatLitInt 4]], ReturnStmt (Just (LitInt 42)))
                                        , (PatArray [PatOr [PatLitInt 5, PatLitInt 6], PatOr [PatLitInt 7, PatLitInt 8]], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec retour complexe
  it "parses match statement with complex return expressions" $ do
    let input = "match x { 1 => return (x + y) * z, 2 => return foo(x, y, z) }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (BinArith Multiply (BinArith Add (Var "x") (Var "y")) (Var "z"))))
                                        , (PatLitInt 2, ReturnStmt (Just (FuncCall (Var "foo") [Var "x", Var "y", Var "z"]))) ])

  -- Tests pour match avec des motifs imbriqués
  it "parses match statement with nested patterns" $ do
    let input = "match x { (1, (2, 3)) => return 42, (4, (5, 6)) => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatTuple [PatLitInt 1, PatTuple [PatLitInt 2, PatLitInt 3]], ReturnStmt (Just (LitInt 42)))
                                        , (PatTuple [PatLitInt 4, PatTuple [PatLitInt 5, PatLitInt 6]], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec les combinaisons de return et expr
  it "parses match statement with mixed return and expression statements" $ do
    let input = "match x { 1 => return 42, 2 => x + 1 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (LitInt 42)))
                                        , (PatLitInt 2, ExprStmt (BinArith Add (Var "x") (LitInt 1))) ])

  -- Tests pour match avec un retour dans un bloc
  it "parses match statement with block containing return statement" $ do
    let input = "match x { 1 => { let a = 10; return a + 1; }, 2 => { let b = 20; return b + 2; } }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, BlockStmt [LetStmt "a" Nothing (LitInt 10), ReturnStmt (Just (BinArith Add (Var "a") (LitInt 1)))])
                                        , (PatLitInt 2, BlockStmt [LetStmt "b" Nothing (LitInt 20), ReturnStmt (Just (BinArith Add (Var "b") (LitInt 2)))]) ])

  -- Tests pour match avec tableau de tuples
  it "parses match statement with array of tuples" $ do
    let input = "match x { [(1, 2), (3, 4)] => return 42, [(5, 6), (7, 8)] => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatArray [PatTuple [PatLitInt 1, PatLitInt 2], PatTuple [PatLitInt 3, PatLitInt 4]], ReturnStmt (Just (LitInt 42)))
                                        , (PatArray [PatTuple [PatLitInt 5, PatLitInt 6], PatTuple [PatLitInt 7, PatLitInt 8]], ReturnStmt (Just (LitInt 43))) ])

  -- Tests pour match avec des fonctions dans les motifs
  it "parses match statement with function call in return" $ do
    let input = "match x { 1 => return foo(42), 2 => return bar(x, y) }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (FuncCall (Var "foo") [LitInt 42])))
                                        , (PatLitInt 2, ReturnStmt (Just (FuncCall (Var "bar") [Var "x", Var "y"]))) ])

  -- Test avec des match simples sans bloc
  it "parses match statement with simple return statements" $ do
    let input = "match x { 1 => return 42, 2 => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (LitInt 42)))
                                        , (PatLitInt 2, ReturnStmt (Just (LitInt 43))) ])

  -- Test avec des match contenant des expressions complexes
  it "parses match statement with complex expressions in return" $ do
    let input = "match x { 1 => return x + y, 2 => return x * y }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (BinArith Add (Var "x") (Var "y"))))
                                        , (PatLitInt 2, ReturnStmt (Just (BinArith Multiply (Var "x") (Var "y")))) ])

  -- Test avec des match et des assignations
  it "parses match statement with variable assignments" $ do
    let input = "match x { 1 => y = 42, 2 => z = 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ExprStmt (Assign (Var "y") (LitInt 42)))
                                        , (PatLitInt 2, ExprStmt (Assign (Var "z") (LitInt 43))) ])

  -- Test avec des match contenant des boucles
  it "parses match statement with for loops in branches" $ do
    let input = "match x { 1 => for i in 1..10 { y = i; }, 2 => for i in 1..=10 { z = i; } }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ForRangeStmt "i" (LitInt 1) (LitInt 10) Exclusive Nothing [ExprStmt (Assign (Var "y") (Var "i"))])
                                        , (PatLitInt 2, ForRangeStmt "i" (LitInt 1) (LitInt 10) Inclusive Nothing [ExprStmt (Assign (Var "z") (Var "i"))]) ])

  -- Test avec des match contenant des conditions if-else
  it "parses match statement with if-else branches" $ do
    let input = "match x { 1 => if (y > 10) { return 42; } else { return 0; }, 2 => if (z == 0) { return 1; } }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, IfStmt (BinComp GreaterThan (Var "y") (LitInt 10))
                                                              (BlockStmt [ReturnStmt (Just (LitInt 42))])
                                                              (Just (BlockStmt [ReturnStmt (Just (LitInt 0))])))
                                        , (PatLitInt 2, IfStmt (BinComp Equal (Var "z") (LitInt 0))
                                                              (BlockStmt [ReturnStmt (Just (LitInt 1))])
                                                              Nothing) ])

  -- Test avec des match utilisant des tableaux
  it "parses match statement with array patterns and returns" $ do
    let input = "match x { [1, 2] => return 42, [3, 4] => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatArray [PatLitInt 1, PatLitInt 2], ReturnStmt (Just (LitInt 42)))
                                        , (PatArray [PatLitInt 3, PatLitInt 4], ReturnStmt (Just (LitInt 43))) ])

  -- Test avec des match et des tuples
  it "parses match statement with tuple patterns" $ do
    let input = "match x { (1, 2) => return 42, (3, 4) => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatTuple [PatLitInt 1, PatLitInt 2], ReturnStmt (Just (LitInt 42)))
                                        , (PatTuple [PatLitInt 3, PatLitInt 4], ReturnStmt (Just (LitInt 43))) ])

  -- Test avec des match contenant des plages de valeurs
  it "parses match statement with range patterns" $ do
    let input = "match x { 1..5 => return 42, 6..10 => return 43 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatRange (PatLitInt 1) (PatLitInt 5), ReturnStmt (Just (LitInt 42)))
                                        , (PatRange (PatLitInt 6) (PatLitInt 10), ReturnStmt (Just (LitInt 43))) ])

  -- Test avec des match et des expressions logiques
  it "parses match statement with logical expressions in return" $ do
    let input = "match x { true => return y && z, false => return y || z }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitBool True, ReturnStmt (Just (BinLogic And (Var "y") (Var "z"))))
                                        , (PatLitBool False, ReturnStmt (Just (BinLogic Or (Var "y") (Var "z")))) ])


  -- Test avec match et une fonction appelée dans les branches
  it "parses match statement with function call in return" $ do
    let input = "match x { 1 => return foo(42), 2 => return bar(43) }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (FuncCall (Var "foo") [LitInt 42])))
                                        , (PatLitInt 2, ReturnStmt (Just (FuncCall (Var "bar") [LitInt 43]))) ])

  -- Test avec des expressions ternaires dans un match
  it "parses match statement with ternary expressions" $ do
    let input = "match x { 1 => return y > 10 ? 42 : 0, 2 => return z == 0 ? 1 : -1 }"
    parseStmt input `shouldBe` Right (MatchStmt (Var "x")
                                        [ (PatLitInt 1, ReturnStmt (Just (Ternary (BinComp GreaterThan (Var "y") (LitInt 10))
                                                                                  (LitInt 42) (LitInt 0))))
                                        , (PatLitInt 2, ReturnStmt (Just (Ternary (BinComp Equal (Var "z") (LitInt 0))
                                                                                  (LitInt 1) (LitInt (-1))))) ])

    -- Tests pour la déclaration de fonction sans corps
  it "parses function declaration without a body" $ do
    let input = "fn add(a: int, b: int) -> int;"
    parseStmt input `shouldBe` Right (FuncDeclStmt "add" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt Nothing)

    -- Tests pour la déclaration de fonction avec un corps
  it "parses function declaration with a body" $ do
    let input = "fn add(a: int, b: int) -> int { return a + b; }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "add" [("a", TInt, Nothing), ("b", TInt, Nothing)] TInt (Just (BlockStmt [ReturnStmt (Just (BinArith Add (Var "a") (Var "b")))])))

    -- Tests pour la fonction avec des paramètres ayant des valeurs par défaut
  it "parses function declaration with default parameter values" $ do
    let input = "fn add(a: int = 10, b: int = 20) -> int { return a + b; }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "add" [("a", TInt, Just (LitInt 10)), ("b", TInt, Just (LitInt 20))] TInt (Just (BlockStmt [ReturnStmt (Just (BinArith Add (Var "a") (Var "b")))])))

    -- Tests pour la déclaration de fonction avec des types de retour complexes
  it "parses function declaration with a complex return type (tuple)" $ do
    let input = "fn getPair() -> (int, string) { return (42, \"hello\"); }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "getPair" [] (TTuple [TInt, TString]) (Just (BlockStmt [ReturnStmt (Just (LitTuple [LitInt 42, LitString "hello"]))])))

    -- Tests pour la fonction sans paramètres
  it "parses function declaration without parameters" $ do
    let input = "fn foo() -> void { return; }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "foo" [] TVoid (Just (BlockStmt [ReturnStmt Nothing])))

    -- Tests pour la déclaration de fonction avec un corps vide
  it "parses function declaration with an empty body" $ do
    let input = "fn doNothing() -> void {}"
    parseStmt input `shouldBe` Right (FuncDeclStmt "doNothing" [] TVoid (Just (BlockStmt [])))

    -- Tests pour la déclaration de fonction avec des types tableau
  it "parses function declaration with array parameters and return type" $ do
    let input = "fn processArray(arr: int[]) -> int[] { return arr; }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "processArray" [("arr", TArray TInt, Nothing)] (TArray TInt) (Just (BlockStmt [ReturnStmt (Just (Var "arr"))])))

    -- Tests pour la déclaration de fonction avec des conditions dans le corps
  it "parses function declaration with if-else in the body" $ do
    let input = "fn checkValue(x: int) -> int { if (x > 10) { return 1; } else { return 0; } }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "checkValue" [("x", TInt, Nothing)] TInt (Just (BlockStmt [IfStmt (BinComp GreaterThan (Var "x") (LitInt 10)) (BlockStmt [ReturnStmt (Just (LitInt 1))]) (Just (BlockStmt [ReturnStmt (Just (LitInt 0))]))])))

    -- Tests pour la fonction avec une boucle dans le corps
  it "parses function declaration with a loop in the body" $ do
    let input = "fn sumTo(n: int) -> int { let sum = 0; for (let i = 0; i < n; i = i + 1) { sum = sum + i; } return sum; }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "sumTo" [("n", TInt, Nothing)] TInt (Just (BlockStmt [LetStmt "sum" Nothing (LitInt 0), ForClassicStmt (Just (LetStmt "i" Nothing (LitInt 0))) (BinComp LessThan (Var "i") (Var "n")) (Just (Assign (Var "i") (BinArith Add (Var "i") (LitInt 1)))) [ExprStmt (Assign (Var "sum") (BinArith Add (Var "sum") (Var "i")))], ReturnStmt (Just (Var "sum"))])))

    -- Tests pour la fonction avec une fonction imbriquée dans le corps
  it "parses function declaration with nested function call" $ do
    let input = "fn outer(a: int) -> int { return inner(a); }"
    parseStmt input `shouldBe` Right (FuncDeclStmt "outer" [("a", TInt, Nothing)] TInt (Just (BlockStmt [ReturnStmt (Just (FuncCall (Var "inner") [Var "a"]))])))

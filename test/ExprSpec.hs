module ExprSpec (spec) where

import Test.Hspec
import Parser.Expr (parseExpr, Expr(..), ArithOp(..), CompOp(..), LogicOp(..))

spec :: Spec
spec = do
  describe "Expression parsing" $ do

    it "parses literal integer" $ do
      let input = "42"
      parseExpr input `shouldBe` Right (LitInt 42)

    it "parses literal string" $ do
      let input = "\"hello\""
      parseExpr input `shouldBe` Right (LitString "hello")

    it "parses literal boolean" $ do
      let input = "true"
      parseExpr input `shouldBe` Right (LitBool True)

    it "parses variable" $ do
      let input = "x"
      parseExpr input `shouldBe` Right (Var "x")

    it "parses binary addition" $ do
      let input = "x + 5"
      parseExpr input `shouldBe` Right (BinArith Add (Var "x") (LitInt 5))

    it "parses comparison" $ do
      let input = "(x == 5)"
      parseExpr input `shouldBe` Right (BinComp Equal (Var "x") (LitInt 5))

    it "parses function call with arguments" $ do
      let input = "f(x, 42)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [Var "x", LitInt 42])

    it "parses logical and" $ do
      let input = "x && y"
      parseExpr input `shouldBe` Right (BinLogic And (Var "x") (Var "y"))

    it "parses ternary expression" $ do
      let input = "x > 5 ? 42 : 0"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (LitInt 42) (LitInt 0))

    it "parses array literals" $ do
      let input = "[1, 2, 3]"
      parseExpr input `shouldBe` Right (LitArray [LitInt 1, LitInt 2, LitInt 3])

    it "parses tuple literals" $ do
      let input = "(1, 2, 3)"
      parseExpr input `shouldBe` Right (LitTuple [LitInt 1, LitInt 2, LitInt 3])

    it "parses multiple binary operations with correct precedence" $ do
      let input = "x + 5 * 3"
      parseExpr input `shouldBe` Right (BinArith Add (Var "x") (BinArith Multiply (LitInt 5) (LitInt 3)))

    it "parses logical expressions with parentheses" $ do
      let input = "(x && y) || z"
      parseExpr input `shouldBe` Right (BinLogic Or (BinLogic And (Var "x") (Var "y")) (Var "z"))

    it "parses nested function calls" $ do
      let input = "f(g(x), 42)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [FuncCall (Var "g") [Var "x"], LitInt 42])

    it "parses combined comparison operators" $ do
      let input = "x > 5 && y < 10"
      parseExpr input `shouldBe` Right (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 5)) (BinComp LessThan (Var "y") (LitInt 10)))

    it "parses nested ternary expressions" $ do
      let input = "x > 5 ? (y > 0 ? 1 : 2) : 0"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (Ternary (BinComp GreaterThan (Var "y") (LitInt 0)) (LitInt 1) (LitInt 2))
                                                (LitInt 0))

    it "parses function call with complex arguments" $ do
      let input = "f(x + 1, y * 2)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [BinArith Add (Var "x") (LitInt 1), BinArith Multiply (Var "y") (LitInt 2)])

    it "parses tuple with arithmetic operations" $ do
      let input = "(1 + 2, 3 * 4, 5 - 6)"
      parseExpr input `shouldBe` Right (LitTuple [BinArith Add (LitInt 1) (LitInt 2),
                                                  BinArith Multiply (LitInt 3) (LitInt 4),
                                                  BinArith Subtract (LitInt 5) (LitInt 6)])

    it "parses tuple with logical and comparison operations" $ do
      let input = "(x > 5, y && z, 3 <= 4)"
      parseExpr input `shouldBe` Right (LitTuple [BinComp GreaterThan (Var "x") (LitInt 5),
                                                  BinLogic And (Var "y") (Var "z"),
                                                  BinComp LessEqual (LitInt 3) (LitInt 4)])

    it "parses nested tuple with operations" $ do
      let input = "((1 + 2, 3 * 4), (5, 6 - 1))"
      parseExpr input `shouldBe` Right (LitTuple [LitTuple [BinArith Add (LitInt 1) (LitInt 2),
                                                            BinArith Multiply (LitInt 3) (LitInt 4)],
                                                  LitTuple [LitInt 5, BinArith Subtract (LitInt 6) (LitInt 1)]])

    it "parses tuple with function calls and operations" $ do
      let input = "(f(x), y + 2, z * g(3))"
      parseExpr input `shouldBe` Right (LitTuple [FuncCall (Var "f") [Var "x"],
                                                  BinArith Add (Var "y") (LitInt 2),
                                                  BinArith Multiply (Var "z") (FuncCall (Var "g") [LitInt 3])])

    it "parses tuple with ternary expressions" $ do
      let input = "(x > 5 ? 1 : 2, y < 3 ? 4 : 5)"
      parseExpr input `shouldBe` Right (LitTuple [Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (LitInt 1) (LitInt 2),
                                                  Ternary (BinComp LessThan (Var "y") (LitInt 3)) (LitInt 4) (LitInt 5)])

    it "parses list with arithmetic operations" $ do
      let input = "[1 + 2, 3 * 4, 5 - 6]"
      parseExpr input `shouldBe` Right (LitArray [BinArith Add (LitInt 1) (LitInt 2),
                                                  BinArith Multiply (LitInt 3) (LitInt 4),
                                                  BinArith Subtract (LitInt 5) (LitInt 6)])

    it "parses list with logical and comparison operations" $ do
      let input = "[x > 5, y && z, 3 <= 4]"
      parseExpr input `shouldBe` Right (LitArray [BinComp GreaterThan (Var "x") (LitInt 5),
                                                  BinLogic And (Var "y") (Var "z"),
                                                  BinComp LessEqual (LitInt 3) (LitInt 4)])

    it "parses nested list with operations" $ do
      let input = "[[1 + 2, 3 * 4], [5, 6 - 1]]"
      parseExpr input `shouldBe` Right (LitArray [LitArray [BinArith Add (LitInt 1) (LitInt 2),
                                                            BinArith Multiply (LitInt 3) (LitInt 4)],
                                                  LitArray [LitInt 5, BinArith Subtract (LitInt 6) (LitInt 1)]])

    it "parses list with function calls and operations" $ do
      let input = "[f(x), y + 2, z * g(3)]"
      parseExpr input `shouldBe` Right (LitArray [FuncCall (Var "f") [Var "x"],
                                                  BinArith Add (Var "y") (LitInt 2),
                                                  BinArith Multiply (Var "z") (FuncCall (Var "g") [LitInt 3])])

    it "parses list with ternary expressions" $ do
      let input = "[x > 5 ? 1 : 2, y < 3 ? 4 : 5]"
      parseExpr input `shouldBe` Right (LitArray [Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (LitInt 1) (LitInt 2),
                                                  Ternary (BinComp LessThan (Var "y") (LitInt 3)) (LitInt 4) (LitInt 5)])

    it "parses deeply nested lists with operations" $ do
      let input = "[[1, 2], [3 * 4, 5 - 6], [f(x), g(7)]]"
      parseExpr input `shouldBe` Right (LitArray [LitArray [LitInt 1, LitInt 2],
                                                  LitArray [BinArith Multiply (LitInt 3) (LitInt 4), BinArith Subtract (LitInt 5) (LitInt 6)],
                                                  LitArray [FuncCall (Var "f") [Var "x"], FuncCall (Var "g") [LitInt 7]]])

    it "parses deeply nested tuples with operations" $ do
      let input = "((1, 2), (3 * 4, 5 - 6), (f(x), g(7)))"
      parseExpr input `shouldBe` Right (LitTuple [LitTuple [LitInt 1, LitInt 2],
                                                  LitTuple [BinArith Multiply (LitInt 3) (LitInt 4), BinArith Subtract (LitInt 5) (LitInt 6)],
                                                  LitTuple [FuncCall (Var "f") [Var "x"], FuncCall (Var "g") [LitInt 7]]])
    
    it "parses subtraction and multiplication with correct precedence" $ do
      let input = "10 - 2 * 3"
      parseExpr input `shouldBe` Right (BinArith Subtract (LitInt 10) (BinArith Multiply (LitInt 2) (LitInt 3)))

    it "parses division and addition with correct precedence" $ do
      let input = "10 / 2 + 3"
      parseExpr input `shouldBe` Right (BinArith Add (BinArith Divide (LitInt 10) (LitInt 2)) (LitInt 3))

    it "parses modulo and subtraction with correct precedence" $ do
      let input = "10 % 3 - 1"
      parseExpr input `shouldBe` Right (BinArith Subtract (BinArith Modulo (LitInt 10) (LitInt 3)) (LitInt 1))

    it "parses chained operations with different precedence" $ do
      let input = "2 + 3 * 4 - 5 / 5"
      parseExpr input `shouldBe` Right (BinArith Subtract (BinArith Add (LitInt 2) (BinArith Multiply (LitInt 3) (LitInt 4)))
                                                        (BinArith Divide (LitInt 5) (LitInt 5)))

    it "parses complex arithmetic with parentheses" $ do
      let input = "(2 + 3) * (4 - 1)"
      parseExpr input `shouldBe` Right (BinArith Multiply (BinArith Add (LitInt 2) (LitInt 3)) (BinArith Subtract (LitInt 4) (LitInt 1)))

    it "parses multiple ternary expressions" $ do
      let input = "x > 5 ? y < 3 ? 1 : 2 : 3"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (Ternary (BinComp LessThan (Var "y") (LitInt 3)) (LitInt 1) (LitInt 2))
                                                (LitInt 3))

    it "parses mixed logical and arithmetic operations" $ do
      let input = "x && y + 1 > z"
      parseExpr input `shouldBe` Right (BinLogic And (Var "x") (BinComp GreaterThan (BinArith Add (Var "y") (LitInt 1)) (Var "z")))

    it "parses mixed comparison and logical operations" $ do
      let input = "(x > 3) && (y < 5) || z == 1"
      parseExpr input `shouldBe` Right (BinLogic Or (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 3)) (BinComp LessThan (Var "y") (LitInt 5)))
                                                    (BinComp Equal (Var "z") (LitInt 1)))

    it "parses nested arithmetic operations with different precedence" $ do
      let input = "1 + (2 * (3 + 4))"
      parseExpr input `shouldBe` Right (BinArith Add (LitInt 1) (BinArith Multiply (LitInt 2) (BinArith Add (LitInt 3) (LitInt 4))))

    it "parses chained comparisons" $ do
      let input = "x > 1 && y < 2 && z >= 3"
      parseExpr input `shouldBe` Right (BinLogic And (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 1)) (BinComp LessThan (Var "y") (LitInt 2)))
                                                    (BinComp GreaterEqual (Var "z") (LitInt 3)))

    it "parses arithmetic followed by comparison" $ do
      let input = "x + 5 > y - 3"
      parseExpr input `shouldBe` Right (BinComp GreaterThan (BinArith Add (Var "x") (LitInt 5)) (BinArith Subtract (Var "y") (LitInt 3)))

    it "parses nested comparisons with logical operations" $ do
      let input = "(x > 5 && y < 10) || z == 3"
      parseExpr input `shouldBe` Right (BinLogic Or (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 5)) (BinComp LessThan (Var "y") (LitInt 10)))
                                                    (BinComp Equal (Var "z") (LitInt 3)))

    it "parses subtraction followed by comparison" $ do
      let input = "x - 1 == 0"
      parseExpr input `shouldBe` Right (BinComp Equal (BinArith Subtract (Var "x") (LitInt 1)) (LitInt 0))

    it "parses multiplication followed by addition" $ do
      let input = "x * 2 + y"
      parseExpr input `shouldBe` Right (BinArith Add (BinArith Multiply (Var "x") (LitInt 2)) (Var "y"))

    it "parses chained ternary expressions" $ do
      let input = "x > 5 ? y > 3 ? 1 : 2 : z > 1 ? 3 : 4"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (Ternary (BinComp GreaterThan (Var "y") (LitInt 3)) (LitInt 1) (LitInt 2))
                                                (Ternary (BinComp GreaterThan (Var "z") (LitInt 1)) (LitInt 3) (LitInt 4)))

    it "parses multiple nested function calls" $ do
      let input = "f(g(h(x, 1)), 2)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [FuncCall (Var "g") [FuncCall (Var "h") [Var "x", LitInt 1]], LitInt 2])

    it "parses arithmetic inside ternary expressions" $ do
      let input = "x > 5 ? y + 1 : z - 2"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (BinArith Add (Var "y") (LitInt 1)) (BinArith Subtract (Var "z") (LitInt 2)))

    it "parses nested parentheses with multiple operations" $ do
      let input = "((x + 2) * (y - 3)) / 2"
      parseExpr input `shouldBe` Right (BinArith Divide (BinArith Multiply (BinArith Add (Var "x") (LitInt 2)) (BinArith Subtract (Var "y") (LitInt 3))) (LitInt 2))

    it "parses logical expressions with multiple comparisons" $ do
      let input = "x > 5 && y < 10 || z == 1"
      parseExpr input `shouldBe` Right (BinLogic Or (BinLogic And (BinComp GreaterThan (Var "x") (LitInt 5)) (BinComp LessThan (Var "y") (LitInt 10)))
                                                    (BinComp Equal (Var "z") (LitInt 1)))

    it "parses complex list literals with operations" $ do
      let input = "[1 + 2, 3 * 4, 5 - 6]"
      parseExpr input `shouldBe` Right (LitArray [BinArith Add (LitInt 1) (LitInt 2), BinArith Multiply (LitInt 3) (LitInt 4), BinArith Subtract (LitInt 5) (LitInt 6)])

    it "parses nested function calls inside ternary expressions" $ do
      let input = "x > 5 ? f(g(1)) : h(2)"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (FuncCall (Var "f") [FuncCall (Var "g") [LitInt 1]]) (FuncCall (Var "h") [LitInt 2]))

    it "parses deeply nested ternary expressions" $ do
      let input = "x > 5 ? y > 3 ? z > 1 ? 1 : 2 : 3 : 4"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (Ternary (BinComp GreaterThan (Var "y") (LitInt 3))
                                                        (Ternary (BinComp GreaterThan (Var "z") (LitInt 1)) (LitInt 1) (LitInt 2))
                                                        (LitInt 3))
                                                (LitInt 4))

    it "parses addition inside a function call argument" $ do
      let input = "f(x + 1)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [BinArith Add (Var "x") (LitInt 1)])

    it "parses logical operations with arithmetic" $ do
      let input = "x && y + 1"
      parseExpr input `shouldBe` Right (BinLogic And (Var "x") (BinArith Add (Var "y") (LitInt 1)))

    it "parses multiple binary arithmetic operations with parentheses" $ do
      let input = "(x + 1) * (y - 2)"
      parseExpr input `shouldBe` Right (BinArith Multiply (BinArith Add (Var "x") (LitInt 1)) (BinArith Subtract (Var "y") (LitInt 2)))

    it "parses arithmetic and comparison inside a function call" $ do
      let input = "f(x + 1 > y)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [BinComp GreaterThan (BinArith Add (Var "x") (LitInt 1)) (Var "y")])

    it "parses complex ternary expression inside a function call" $ do
      let input = "f(x > 5 ? 1 : 0)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (LitInt 1) (LitInt 0)])

    it "parses arithmetic inside nested list literals" $ do
      let input = "[[x + 1, y - 2], [3 * 4, 5 / 6]]"
      parseExpr input `shouldBe` Right (LitArray [LitArray [BinArith Add (Var "x") (LitInt 1), BinArith Subtract (Var "y") (LitInt 2)],
                                                  LitArray [BinArith Multiply (LitInt 3) (LitInt 4), BinArith Divide (LitInt 5) (LitInt 6)]])

    it "parses nested function calls with arithmetic in arguments" $ do
      let input = "f(g(x + 1), h(y - 2))"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [FuncCall (Var "g") [BinArith Add (Var "x") (LitInt 1)], FuncCall (Var "h") [BinArith Subtract (Var "y") (LitInt 2)]])

    it "parses ternary expression inside list literal" $ do
      let input = "[x > 5 ? 1 : 0, y < 3 ? 2 : 3]"
      parseExpr input `shouldBe` Right (LitArray [Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (LitInt 1) (LitInt 0),
                                                  Ternary (BinComp LessThan (Var "y") (LitInt 3)) (LitInt 2) (LitInt 3)])

    it "parses deeply nested function calls" $ do
      let input = "f(g(h(i(j(1)))))"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [FuncCall (Var "g") [FuncCall (Var "h") [FuncCall (Var "i") [FuncCall (Var "j") [LitInt 1]]]]])

    it "parses deeply nested lists with function calls" $ do
      let input = "[[f(x), g(y)], [h(z), k(w)]]"
      parseExpr input `shouldBe` Right (LitArray [LitArray [FuncCall (Var "f") [Var "x"], FuncCall (Var "g") [Var "y"]],
                                                  LitArray [FuncCall (Var "h") [Var "z"], FuncCall (Var "k") [Var "w"]]])

    it "parses logical operation with comparison inside a function call" $ do
      let input = "f(x > 5 && y < 10)"
      parseExpr input `shouldBe` Right (FuncCall (Var "f") [BinLogic And (BinComp GreaterThan (Var "x") (LitInt 5)) (BinComp LessThan (Var "y") (LitInt 10))])

    it "parses complex arithmetic inside ternary expression" $ do
      let input = "x > 5 ? (y + 1) * 2 : (z - 3) / 4"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (BinArith Multiply (BinArith Add (Var "y") (LitInt 1)) (LitInt 2))
                                                (BinArith Divide (BinArith Subtract (Var "z") (LitInt 3)) (LitInt 4)))

    it "parses arithmetic with multiple levels of parentheses" $ do
      let input = "(x + (y * (z - 1))) / 2"
      parseExpr input `shouldBe` Right (BinArith Divide (BinArith Add (Var "x") (BinArith Multiply (Var "y") (BinArith Subtract (Var "z") (LitInt 1)))) (LitInt 2))

    it "parses ternary expression with nested arithmetic" $ do
      let input = "x > 5 ? (y + 2) : (z * 3)"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (BinArith Add (Var "y") (LitInt 2)) (BinArith Multiply (Var "z") (LitInt 3)))

    it "parses deeply nested ternary expressions with operations" $ do
      let input = "x > 5 ? y < 3 ? z * 2 : 3 : 4"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (Ternary (BinComp LessThan (Var "y") (LitInt 3)) (BinArith Multiply (Var "z") (LitInt 2)) (LitInt 3))
                                                (LitInt 4))

    it "parses complex ternary expression with multiple operations" $ do
      let input = "x > 5 ? y + 1 * 3 : z - 2 / 4"
      parseExpr input `shouldBe` Right (Ternary (BinComp GreaterThan (Var "x") (LitInt 5))
                                                (BinArith Add (Var "y") (BinArith Multiply (LitInt 1) (LitInt 3)))
                                                (BinArith Subtract (Var "z") (BinArith Divide (LitInt 2) (LitInt 4))))

    -- Tests pour ArrayIndex
    it "parses array index access" $ do
      let input = "arr[0]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (LitInt 0))

    it "parses array index access with a variable" $ do
      let input = "arr[x]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (Var "x"))

    it "parses nested array index access" $ do
      let input = "matrix[1][2]"
      parseExpr input `shouldBe` Right (ArrayIndex (ArrayIndex (Var "matrix") (LitInt 1)) (LitInt 2))

    it "parses array index with complex expression" $ do
      let input = "arr[x + 1]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (BinArith Add (Var "x") (LitInt 1)))

    it "parses assignment with array index" $ do
      let input = "arr[0] = 42"
      parseExpr input `shouldBe` Right (Assign (ArrayIndex (Var "arr") (LitInt 0)) (LitInt 42))

    it "parses function call inside array index" $ do
      let input = "arr[f(x)]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (FuncCall (Var "f") [Var "x"]))

    it "parses nested array index with function call" $ do
      let input = "matrix[f(x)][g(y)]"
      parseExpr input `shouldBe` Right (ArrayIndex (ArrayIndex (Var "matrix") (FuncCall (Var "f") [Var "x"])) (FuncCall (Var "g") [Var "y"]))

    it "parses array index with ternary expression" $ do
      let input = "arr[x > 5 ? 1 : 0]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (Ternary (BinComp GreaterThan (Var "x") (LitInt 5)) (LitInt 1) (LitInt 0)))

    it "parses assignment to array index with complex expression" $ do
      let input = "arr[x + 1] = 42"
      parseExpr input `shouldBe` Right (Assign (ArrayIndex (Var "arr") (BinArith Add (Var "x") (LitInt 1))) (LitInt 42))

    it "parses array index with logical comparison" $ do
      let input = "arr[x && y]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (BinLogic And (Var "x") (Var "y")))

    it "parses array index access with subtraction" $ do
      let input = "arr[10 - 3]"
      parseExpr input `shouldBe` Right (ArrayIndex (Var "arr") (BinArith Subtract (LitInt 10) (LitInt 3)))

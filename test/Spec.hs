import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)
import Data.Map (fromList)
import qualified Data.Map as Map

import Parser(parseSExpr, parseSExprs, parseInt, parseBool, parseSymbol, parseList)
import AST(SExpr(..), Ast(..), sexprToAST, symbolToString)
import Evaluator(evalAST, initEnv, evalDefine, lookupEnv, expectBool, applyLambda, applyFunction)
import Builtin(evalBuiltinFunction, getInt)

-- parseSExpr Tests
testParseSExprSSymbol :: TestTree
testParseSExprSSymbol = testCase "Symbol Parsing" $
  parse parseSExpr "" "x" @?= Right (SSymbol "x")

testParseSExprSInt :: TestTree
testParseSExprSInt = testCase "Integer Parsing" $
  parse parseSExpr "" "42" @?= Right (SInt 42)

testParseSExprSBoolTrue :: TestTree
testParseSExprSBoolTrue = testCase "Boolean True Parsing" $
  parse parseSExpr "" "#t" @?= Right (SBool True)

testParseSExprSBoolFalse :: TestTree
testParseSExprSBoolFalse = testCase "Boolean False Parsing" $
  parse parseSExpr "" "#f" @?= Right (SBool False)

testParseSExprSList :: TestTree
testParseSExprSList = testCase "List Parsing" $
  parse parseSExpr "" "(x 42 #t #f)" @?= Right (SList [SSymbol "x", SInt 42, SBool True, SBool False])

-- parseSExprs Tests
testParseSExprs :: TestTree
testParseSExprs = testCase "Multiple Expressions Parsing" $
  parse parseSExprs "" "(x 42 #t #f)" @?= Right [SList [SSymbol "x",SInt 42,SBool True,SBool False]]

-- parseInt Tests
testParseInt :: TestTree
testParseInt = testCase "Integer Parsing" $
  parse parseInt "" "42" @?= Right (SInt 42)

testParseIntNegative :: TestTree
testParseIntNegative = testCase "Negative Integer Parsing" $
  parse parseInt "" "-42" @?= Right (SInt (-42))

-- parseBool Tests
testParseBoolTrue :: TestTree
testParseBoolTrue = testCase "Boolean True Parsing" $
  parse parseBool "" "#t" @?= Right (SBool True)

testParseBoolFalse :: TestTree
testParseBoolFalse = testCase "Boolean False Parsing" $
  parse parseBool "" "#f" @?= Right (SBool False)

-- parseSymbol Tests
testParseSymbol :: TestTree
testParseSymbol = testCase "Symbol Parsing" $
  parse parseSymbol "" "x" @?= Right (SSymbol "x")

-- parseList Tests
testParseList :: TestTree
testParseList = testCase "List Parsing" $
  parse parseList "" "(x 42 #t #f)" @?= Right (SList[SSymbol "x", SInt 42, SBool True, SBool False])



-- AST Tests

testSExprDerivingEqSInt :: TestTree
testSExprDerivingEqSInt = testCase "SExpr Deriving Eq" $
  (==) (SInt 2) (SInt 2) @?= True

testSExprDerivingEqSSymbol :: TestTree
testSExprDerivingEqSSymbol = testCase "SExpr Deriving Eq" $
  (==) (SSymbol "x") (SSymbol "x") @?= True

testSExprDerivingEqSList :: TestTree
testSExprDerivingEqSList = testCase "SExpr Deriving Eq" $
  (==) (SList [SSymbol "x", SInt 42]) (SList [SSymbol "x", SInt 42]) @?= True

testSExprDerivingEqSBool :: TestTree
testSExprDerivingEqSBool = testCase "SExpr Deriving Eq" $
  (==) (SBool True) (SBool True) @?= True

testSexprDerivingEqFalse :: TestTree
testSexprDerivingEqFalse = testCase "SExpr Deriving Eq" $
  (==) (SBool True) (SBool False) @?= False

testSexprDerivingShow :: TestTree
testSexprDerivingShow = testCase "SExpr Deriving Show" $
  show (SInt 2) ++ show (SSymbol "x") ++ show (SList[]) ++ show (SBool True) @?= "SInt 2SSymbol \"x\"SList []SBool True"

testASTDerivingEqAstInt :: TestTree
testASTDerivingEqAstInt = testCase "AST Deriving Eq" $
  (==) (AstInt 2) (AstInt 2) @?= True

testASTDerivingEqAstSym :: TestTree
testASTDerivingEqAstSym = testCase "AST Deriving Eq" $
  (==) (AstSym "x") (AstSym "x") @?= True

testASTDerivingEqCall :: TestTree
testASTDerivingEqCall = testCase "AST Deriving Eq" $
  (==) (Call "x" [AstInt 42]) (Call "x" [AstInt 42]) @?= True

testASTDerivingEqDefine :: TestTree
testASTDerivingEqDefine = testCase "AST Deriving Eq" $
  (==) (Define "x" (AstInt 42)) (Define "x" (AstInt 42)) @?= True

testASTDerivingEqLambda :: TestTree
testASTDerivingEqLambda = testCase "AST Deriving Eq" $
  (==) (Lambda ["x"] (AstInt 42)) (Lambda ["x"] (AstInt 42)) @?= True

testASTDerivingEqIf :: TestTree
testASTDerivingEqIf = testCase "AST Deriving Eq" $
  (==) (If (AstBool True) (AstInt 1) (AstInt 2)) (If (AstBool True) (AstInt 1) (AstInt 2)) @?= True

testASTDerivingEqASTBuiltIn :: TestTree
testASTDerivingEqASTBuiltIn = testCase "AST Deriving Eq" $
  (==) (AstBuiltin "+") (AstBuiltin "+") @?= True

testASTDerivingEqCallLambda :: TestTree
testASTDerivingEqCallLambda = testCase "AST Deriving Eq" $
  (==) (CallLambda (Lambda ["x"] (AstInt 42)) [AstInt 1]) (CallLambda (Lambda ["x"] (AstInt 42)) [AstInt 1]) @?= True

testASTDerivingEqFalse :: TestTree
testASTDerivingEqFalse = testCase "AST Deriving Eq" $
  (==) (AstInt 2) (AstInt 3) @?= False

testASTDerivingShow :: TestTree
testASTDerivingShow = testCase "AST Deriving Show" $
  show (AstInt 2) ++ show (AstSym "x") ++ show (Call "x" [AstInt 42]) ++ show (Define "x" (AstInt 42)) ++ show (Lambda ["x"] (AstInt 42)) ++ show (If (AstBool True) (AstInt 1) (AstInt 2)) @?= "AstInt 2AstSym \"x\"Call \"x\" [AstInt 42]Define \"x\" (AstInt 42)Lambda [\"x\"] (AstInt 42)If (AstBool True) (AstInt 1) (AstInt 2)"

testSExprToAST :: TestTree
testSExprToAST = testCase "SExpr to AST" $
  sexprToAST (SList [SSymbol "x", SInt 42]) @?= Right (Call "x" [AstInt 42])

testSExprToASTSymbol :: TestTree
testSExprToASTSymbol = testCase "Symbol SExpr to AST" $
  sexprToAST (SSymbol "x") @?= Right (AstSym "x")

testSExprToASTInt :: TestTree
testSExprToASTInt = testCase "Int SExpr to AST" $
  sexprToAST (SInt 42) @?= Right (AstInt 42)

testSExprToASTBoolTrue :: TestTree
testSExprToASTBoolTrue = testCase "Bool True SExpr to AST" $
  sexprToAST (SBool True) @?= Right (AstBool True)

testSExprToASTBoolFalse :: TestTree
testSExprToASTBoolFalse = testCase "Bool False SExpr to AST" $
  sexprToAST (SBool False) @?= Right (AstBool False)

testSExprToASTList :: TestTree
testSExprToASTList = testCase "List SExpr to AST" $
  sexprToAST (SList [SSymbol "x", SInt 42, SBool True, SBool False]) @?= Right (Call "x" [AstInt 42,AstBool True,AstBool False])

testSExprToASTDefine :: TestTree
testSExprToASTDefine = testCase "Define SExpr to AST" $
  sexprToAST (SList [SSymbol "define", SSymbol "x", SInt 42]) @?= Right (Define "x" (AstInt 42))

testSExprToASTDefineFunction :: TestTree
testSExprToASTDefineFunction = testCase "Define Function SExpr to AST" $
  sexprToAST (SList [SSymbol "define", SList [SSymbol "f", SSymbol "x"], SInt 42]) @?= Right (Define "f" (Lambda ["x"] (AstInt 42)))

testSExprToASTIf :: TestTree
testSExprToASTIf = testCase "If SExpr to AST" $
  sexprToAST (SList [SSymbol "if", SBool True, SInt 42, SInt 0]) @?= Right (If (AstBool True) (AstInt 42) (AstInt 0))

testSExprToASTLambda :: TestTree
testSExprToASTLambda = testCase "Lambda SExpr to AST" $
  sexprToAST (SList [SSymbol "lambda", SList [SSymbol "x"], SInt 42]) @?= Right (Lambda ["x"] (AstInt 42))

testSExprToASTLambdaMultipleArgs :: TestTree
testSExprToASTLambdaMultipleArgs = testCase "Lambda SExpr to AST with multiple args" $
  sexprToAST (SList [SSymbol "lambda", SList [SSymbol "x", SSymbol "y"], SInt 42]) @?= Right (Lambda ["x", "y"] (AstInt 42))

testSExprToASTReservedKeyword :: TestTree
testSExprToASTReservedKeyword = testCase "Reserved keyword to AST" $
  sexprToAST (SSymbol "define") @?= Left "Error: 'define' is a reserved keyword and cannot be used as a variable."

testSExprToASTOperator :: TestTree
testSExprToASTOperator = testCase "Operator to AST" $
  sexprToAST (SList [SSymbol "+", SInt 1, SInt 2]) @?= Right (Call "+" [AstInt 1, AstInt 2])

testSExprToASTCallLambda:: TestTree
testSExprToASTCallLambda = testCase "Lambda Application SExpr to AST" $
  sexprToAST (SList [SList [SSymbol "lambda", SList [SSymbol "x"], SInt 42], SInt 1]) @?= Right (CallLambda (Lambda ["x"] (AstInt 42)) [AstInt 1])

-- Symbol to String Tests
testSymbolToString :: TestTree
testSymbolToString = testCase "Symbol to String" $
  symbolToString (SSymbol "x") @?= Right "x"

testSymbolToStringError :: TestTree
testSymbolToStringError = testCase "Symbol to String Error" $
  symbolToString (SInt 42) @?= Left "Expected a symbol"



-- Evaluator Tests

testInitEnv :: TestTree
testInitEnv = testCase "Init Env" $
  initEnv @?= fromList [("*",AstBuiltin "*"),("+",AstBuiltin "+"),("-",AstBuiltin "-"),("<",AstBuiltin "<"),("<=",AstBuiltin "<="),(">",AstBuiltin ">"),(">=",AstBuiltin ">="),("div",AstBuiltin "div"),("eq?",AstBuiltin "eq?"),("mod",AstBuiltin "mod")]

testLookupEnv :: TestTree
testLookupEnv = testCase "Lookup Env" $
  lookupEnv "x" (fromList [("x",AstInt 42)]) @?= Right (AstInt 42)

testLookupEnvError :: TestTree
testLookupEnvError = testCase "Lookup Env Error" $
  lookupEnv "x" (fromList []) @?= Left "Error: variable 'x' is not bound."

testExpectBool :: TestTree
testExpectBool = testCase "Expect Bool" $
  expectBool (AstBool True) @?= Right True

testExpectBoolError :: TestTree
testExpectBoolError = testCase "Expect Bool Error" $
  expectBool (AstInt 42) @?= Left "Error: condition in 'if' must evaluate to a boolean."

-- Test applyLambda
testApplyLambda :: TestTree
testApplyLambda = testGroup "applyLambda Tests"
  [ testCase "Correct number of arguments" $
      applyLambda Map.empty ["x"] (AstInt 1) [AstInt 42] @?= Right (AstInt 1)
  , testCase "Incorrect number of arguments" $
      applyLambda Map.empty ["x"] (AstInt 1) [] @?= Left "Error: incorrect number of arguments."
  ]

-- Test applyFunction
testApplyFunction :: TestTree
testApplyFunction = testGroup "applyFunction Tests"
  [ testCase "Apply built-in function" $
      applyFunction Map.empty (AstBuiltin "+") [AstInt 1, AstInt 2] @?= evalBuiltinFunction "+" [AstInt 1, AstInt 2]
  , testCase "Apply lambda function" $
      applyFunction Map.empty (Lambda ["x"] (AstInt 1)) [AstInt 42] @?= applyLambda Map.empty ["x"] (AstInt 1) [AstInt 42]
  , testCase "Error on non-function" $
      applyFunction Map.empty (AstInt 1) [] @?= Left "Error: trying to call a non-function."
  ]

-- Test evalAST
testEvalAST :: TestTree
testEvalAST = testGroup "evalAST Tests"
  [ testCase "Evaluate integer" $
      evalAST Map.empty (AstInt 42) @?= Right (AstInt 42)
  , testCase "Evaluate boolean" $
      evalAST Map.empty (AstBool True) @?= Right (AstBool True)
  , testCase "Evaluate symbol" $
      evalAST (Map.fromList [("x", AstInt 42)]) (AstSym "x") @?= Right (AstInt 42)
  , testCase "Evaluate function call" $
      evalAST (Map.fromList [("f", Lambda ["x"] (AstInt 1))]) (Call "f" [AstInt 42]) @?= Right (AstInt 1)
  , testCase "Evaluate lambda call" $
      evalAST Map.empty (CallLambda (Lambda ["x"] (AstInt 1)) [AstInt 42]) @?= Right (AstInt 1)
  , testCase "Error on non-lambda call" $
      evalAST Map.empty (CallLambda (AstInt 1) [AstInt 42]) @?= Left "Error: trying to call a non-lambda expression."
  , testCase "Evaluate list of expressions" $
      evalAST Map.empty (AstList [AstInt 1, AstInt 2]) @?= Right (AstList [AstInt 1, AstInt 2])
  , testCase "Evaluate lambda" $
      evalAST Map.empty (Lambda ["x"] (AstInt 1)) @?= Right (Lambda ["x"] (AstInt 1))
  , testCase "Evaluate definition" $
      evalAST Map.empty (Define "x" (AstInt 42)) @?= Right (Define "x" (AstInt 42))
  , testCase "Evaluate condition" $
      evalAST Map.empty (If (AstBool True) (AstInt 1) (AstInt 2)) @?= Right (AstInt 1)
  , testCase "Error on built-in function evaluation" $
      evalAST Map.empty (AstBuiltin "+") @?= Left "Error: built-in function cannot be evaluated directly."
  , testCase "Reserved Keyword Error" $
      sexprToAST (SSymbol "define") @?= Left "Error: 'define' is a reserved keyword and cannot be used as a variable."
  , testCase "Symbol AST" $
      sexprToAST (SSymbol "x") @?= Right (AstSym "x")
  , testCase "Built-in operator call" $
      sexprToAST (SList [SSymbol "+", SInt 1, SInt 2]) @?= Right (Call "+" [AstInt 1, AstInt 2])
  ]

testEvalDefine :: TestTree
testEvalDefine = testCase "Evaluate Define" $
  evalDefine (Define "x" (AstInt 42)) Map.empty @?= Right (Map.fromList [("x", AstInt 42)])

testEvalDefineError :: TestTree
testEvalDefineError = testCase "Evaluate Define Error" $
  evalDefine (AstList []) (Map.empty) @?= Right (fromList [])



-- Builtin Tests

testEvalBuiltinFunction :: TestTree
testEvalBuiltinFunction = testGroup "evalBuiltinFunction Tests"
  [ testCase "getInt Error" $
      getInt (AstSym "1") @?= Left "Expected an integer"
  , testCase "Addition with valid arguments" $
      evalBuiltinFunction "+" [AstInt 1, AstInt 2, AstInt 3] @?= Right (AstInt 6)
  , testCase "Addition with no arguments" $
      evalBuiltinFunction "+" [] @?= Left "Addition requires at least one argument"
  , testCase "Subtraction with valid arguments" $
      evalBuiltinFunction "-" [AstInt 10, AstInt 3, AstInt 2] @?= Right (AstInt 5)
  , testCase "Subtraction with no arguments" $
      evalBuiltinFunction "-" [] @?= Left "Subtraction requires at least one argument"
  , testCase "Multiplication with valid arguments" $
      evalBuiltinFunction "*" [AstInt 2, AstInt 3, AstInt 4] @?= Right (AstInt 24)
  , testCase "Multiplication with no arguments" $
      evalBuiltinFunction "*" [] @?= Left "Multiplication requires at least one argument"
  , testCase "Division with valid arguments" $
      evalBuiltinFunction "div" [AstInt 20, AstInt 2, AstInt 2] @?= Right (AstInt 5)
  , testCase "Division by zero" $
      evalBuiltinFunction "div" [AstInt 20, AstInt 0] @?= Left "Division by zero error"
  , testCase "Division with no arguments" $
      evalBuiltinFunction "div" [] @?= Left "Division requires at least two arguments"
  , testCase "Modulo with valid arguments" $
      evalBuiltinFunction "mod" [AstInt 20, AstInt 3] @?= Right (AstInt 2)
  , testCase "Modulo by zero" $
      evalBuiltinFunction "mod" [AstInt 20, AstInt 0] @?= Left "Modulo by zero error"
  , testCase "Modulo with no arguments" $
      evalBuiltinFunction "mod" [] @?= Left "Modulo requires exactly two arguments"
  , testCase "Less than with valid arguments" $
      evalBuiltinFunction "<" [AstInt 1, AstInt 2] @?= Right (AstBool True)
  , testCase "Less than with invalid arguments" $
      evalBuiltinFunction "<" [AstInt 1] @?= Left "Less than requires two arguments"
  , testCase "Greater than with valid arguments" $
      evalBuiltinFunction ">" [AstInt 3, AstInt 2] @?= Right (AstBool True)
  , testCase "Greater than with invalid arguments" $
      evalBuiltinFunction ">" [AstInt 3] @?= Left "Greater than requires two arguments"
  , testCase "Less than or equal to with valid arguments" $
      evalBuiltinFunction "<=" [AstInt 2, AstInt 2] @?= Right (AstBool True)
  , testCase "Less than or equal to with invalid arguments" $
      evalBuiltinFunction "<=" [AstInt 2] @?= Left "Less than or equal to requires two arguments"
  , testCase "Greater than or equal to with valid arguments" $
      evalBuiltinFunction ">=" [AstInt 3, AstInt 2] @?= Right (AstBool True)
  , testCase "Greater than or equal to with invalid arguments" $
      evalBuiltinFunction ">=" [AstInt 3] @?= Left "Greater than or equal to requires two arguments"
  , testCase "Equality check with valid arguments" $
      evalBuiltinFunction "eq?" [AstInt 2, AstInt 2] @?= Right (AstBool True)
  , testCase "Equality check with invalid arguments" $
      evalBuiltinFunction "eq?" [AstInt 2] @?= Left "Equality check requires two arguments"
  , testCase "Unknown function" $
      evalBuiltinFunction "unknown" [AstInt 1] @?= Left "Unknown function: unknown"
  ]

-- Main
main :: IO ()
main = defaultMain $ testGroup "S-Expression Tests"
  [ testGroup "S-Expression Parsing"
      [ testParseSExprSSymbol 
      , testParseSExprSInt
      , testParseSExprSBoolTrue
      , testParseSExprSBoolFalse
      , testParseSExprSList
      , testParseSExprs
      ]
  , testGroup "Integer Parsing"
      [ testParseInt
      , testParseIntNegative
      ]
  , testGroup "Boolean Parsing"
      [ testParseBoolTrue
      , testParseBoolFalse
      ]
  , testGroup "Symbol Parsing"
      [ testParseSymbol
      ]
  , testGroup "List Parsing"
      [ testParseList
      ]
  , testGroup "AST"
      [ testSExprToAST
      , testSExprToASTSymbol
      , testSExprToASTInt
      , testSExprToASTBoolTrue
      , testSExprToASTBoolFalse
      , testSExprToASTList
      , testSExprToASTDefine
      , testSExprToASTDefineFunction
      , testSExprToASTIf
      , testSExprToASTLambda
      , testSExprToASTLambdaMultipleArgs
      , testSExprToASTReservedKeyword
      , testSExprToASTOperator
      , testSExprToASTCallLambda
      , testSExprDerivingEqSInt
      , testSExprDerivingEqSSymbol
      , testSExprDerivingEqSList
      , testSExprDerivingEqSBool
      , testSexprDerivingEqFalse
      , testSexprDerivingShow
      , testASTDerivingEqAstInt
      , testASTDerivingEqAstSym
      , testASTDerivingEqCall
      , testASTDerivingEqDefine
      , testASTDerivingEqLambda
      , testASTDerivingEqIf
      , testASTDerivingEqASTBuiltIn
      , testASTDerivingEqCallLambda
      , testASTDerivingEqFalse
      , testASTDerivingShow
      ]
  , testGroup "Symbol to String"
      [ testSymbolToString
      , testSymbolToStringError
      ]
  , testGroup "Evaluator"
      [ testInitEnv
      , testLookupEnv
      , testLookupEnvError
      , testExpectBool
      , testExpectBoolError
      , testApplyLambda
      , testApplyFunction
      , testEvalAST
      , testEvalDefine
      , testEvalDefineError
      ]
  , testGroup "Builtin"
      [ testEvalBuiltinFunction
      ]
  ]

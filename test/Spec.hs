import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)

import Parser(parseSExpr, parseSExprs, parseInt, parseBool, parseSymbol, parseList)
import AST(SExpr(..))

-- parseSExpr Tests
testParseSExprSSymbol = testCase "Symbol Parsing" $
  parse parseSExpr "" "x" @?= Right (SSymbol "x")

testParseSExprSInt = testCase "Integer Parsing" $
  parse parseSExpr "" "42" @?= Right (SInt 42)

testParseSExprSBoolTrue = testCase "Boolean True Parsing" $
  parse parseSExpr "" "#t" @?= Right (SBool True)

testParseSExprSBoolFalse = testCase "Boolean False Parsing" $
  parse parseSExpr "" "#f" @?= Right (SBool False)

testParseSExprEmpty = testCase "Empty Parsing" $
  parse parseSExpr "" "" @?= Right (SList [])

testParseSExprSList = testCase "List Parsing" $
  parse parseSExpr "" "(x 42 #t #f)" @?= Right (SList [SSymbol "x", SInt 42, SBool True, SBool False])

-- parseSExprs Tests
testParseSExprs = testCase "Multiple Expressions Parsing" $
  parse parseSExprs "" "(x 42 #t #f)" @?= Right [SList [SSymbol "x",SInt 42,SBool True,SBool False]]

testParseSExprsEmpty = testCase "Empty Parsing" $
  parse parseSExprs "" "" @?= Right []

-- parseInt Tests
testParseInt = testCase "Integer Parsing" $
  parse parseInt "" "42" @?= Right (SInt 42)

testParseIntNegative = testCase "Negative Integer Parsing" $
  parse parseInt "" "-42" @?= Right (SInt (-42))

-- parseBool Tests
testParseBoolTrue = testCase "Boolean True Parsing" $
  parse parseBool "" "#t" @?= Right (SBool True)

testParseBoolFalse = testCase "Boolean False Parsing" $
  parse parseBool "" "#f" @?= Right (SBool False)

-- parseSymbol Tests
testParseSymbol = testCase "Symbol Parsing" $
  parse parseSymbol "" "x" @?= Right (SSymbol "x")

-- parseList Tests
testParseList = testCase "List Parsing" $
  parse parseList "" "(x 42 #t #f)" @?= Right (SList[SSymbol "x", SInt 42, SBool True, SBool False])

-- Main
main :: IO ()
main = defaultMain $ testGroup "S-Expression Tests"
  [ testParseSExprSSymbol 
  , testParseSExprSInt
  , testParseSExprSBoolTrue
  , testParseSExprSBoolFalse
  , testParseSExprEmpty
  , testParseSExprSList
  , testParseSExprs
  , testParseSExprsEmpty
  , testParseInt
  , testParseIntNegative
  , testParseBoolTrue
  , testParseBoolFalse
  , testParseSymbol
  , testParseList
  ]


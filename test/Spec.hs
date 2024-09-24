import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)

import Parser(parseSExpr)
import AST(SExpr(..))

-- Tests individuels
testParseSExprSSymbol = testCase "Symbol Parsing" $
  parse parseSExpr "" "x" @?= Right (SSymbol "x")

testParseSExprSInt = testCase "Integer Parsing" $
  parse parseSExpr "" "42" @?= Right (SInt 42)

testParseSExprSBoolTrue = testCase "Boolean True Parsing" $
  parse parseSExpr "" "#t" @?= Right (SBool True)

testParseSExprSBoolFalse = testCase "Boolean False Parsing" $
  parse parseSExpr "" "#f" @?= Right (SBool False)

main :: IO ()
main = defaultMain $ testGroup "S-Expression Tests"
  [ testParseSExprSSymbol
  , testParseSExprSInt
  , testParseSExprSBoolTrue
  , testParseSExprSBoolFalse
  ]


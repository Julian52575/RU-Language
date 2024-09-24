import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)

import Parser(parseSExpr, parseSExprs, parseInt, parseBool, parseSymbol, parseList)
import AST(SExpr(..), Ast(..), sexprToAST, symbolToString)

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

testParseSExprEmpty :: TestTree
testParseSExprEmpty = testCase "Empty Parsing" $
  parse parseSExpr "" "" @?= Right (SList [])

testParseSExprSList :: TestTree
testParseSExprSList = testCase "List Parsing" $
  parse parseSExpr "" "(x 42 #t #f)" @?= Right (SList [SSymbol "x", SInt 42, SBool True, SBool False])

-- parseSExprs Tests
testParseSExprs :: TestTree
testParseSExprs = testCase "Multiple Expressions Parsing" $
  parse parseSExprs "" "(x 42 #t #f)" @?= Right [SList [SSymbol "x",SInt 42,SBool True,SBool False]]

testParseSExprsEmpty :: TestTree
testParseSExprsEmpty = testCase "Empty Parsing" $
  parse parseSExprs "" "" @?= Right []

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
testSExprToAST :: TestTree
testSExprToAST = testCase "SExpr to AST" $
  sexprToAST (SList [SSymbol "x", SInt 42]) @?= Right (Call "x" [AstInt 42])

testSExprToASTEmpty :: TestTree
testSExprToASTEmpty = testCase "Empty SExpr to AST" $
  sexprToAST (SList []) @?= Right (AstList [])

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

testSymbolToString :: TestTree
testSymbolToString = testCase "Symbol to String" $
  symbolToString (SSymbol "x") @?= Right "x"

testSymbolToStringError :: TestTree
testSymbolToStringError = testCase "Symbol to String Error" $
  symbolToString (SInt 42) @?= Left "Expected a symbol"

-- Main
main :: IO ()
main = defaultMain $ testGroup "S-Expression Tests"
  [ testGroup "S-Expression Parsing"
      [ testParseSExprSSymbol 
      , testParseSExprSInt
      , testParseSExprSBoolTrue
      , testParseSExprSBoolFalse
      , testParseSExprEmpty
      , testParseSExprSList
      , testParseSExprs
      , testParseSExprsEmpty
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
      , testSExprToASTEmpty
      , testSExprToASTSymbol
      , testSExprToASTInt
      , testSExprToASTBoolTrue
      , testSExprToASTBoolFalse
      , testSExprToASTList
      ]
  , testGroup "Symbol to String"
      [ testSymbolToString
      , testSymbolToStringError
      ]
  ]

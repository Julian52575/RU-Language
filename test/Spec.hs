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

-- Symbol to String Tests
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
      ]
  , testGroup "Symbol to String"
      [ testSymbolToString
      , testSymbolToStringError
      ]
  ]

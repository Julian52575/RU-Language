module PatternSpec (spec) where

import Test.Hspec
import Parser.Pattern (patternExpr, Pattern(..))
import Text.Megaparsec (parse, errorBundlePretty)

-- Utility to parse patterns easily
parsePattern :: String -> Either String Pattern
parsePattern input = case parse patternExpr "" input of
    Left err -> Left (errorBundlePretty err)
    Right pat -> Right pat

spec :: Spec
spec = do
  describe "Pattern parsing" $ do

    it "parses literal integer pattern" $ do
      let input = "42"
      parsePattern input `shouldBe` Right (PatLitInt 42)

    it "parses literal boolean pattern (true)" $ do
      let input = "true"
      parsePattern input `shouldBe` Right (PatLitBool True)

    it "parses literal boolean pattern (false)" $ do
      let input = "false"
      parsePattern input `shouldBe` Right (PatLitBool False)

    it "parses literal string pattern" $ do
      let input = "\"hello\""
      parsePattern input `shouldBe` Right (PatLitString "hello")

    it "parses wildcard pattern" $ do
      let input = "_"
      parsePattern input `shouldBe` Right PatWildcard

    it "parses variable pattern" $ do
      let input = "x"
      parsePattern input `shouldBe` Right (PatVar "x")

    it "parses tuple pattern with literals" $ do
      let input = "(1, true, \"hello\")"
      parsePattern input `shouldBe` Right (PatTuple [PatLitInt 1, PatLitBool True, PatLitString "hello"])

    it "parses tuple pattern with variables and literals" $ do
      let input = "(x, 42, _)"
      parsePattern input `shouldBe` Right (PatTuple [PatVar "x", PatLitInt 42, PatWildcard])

    it "parses array pattern with literals" $ do
      let input = "[1, 2, 3]"
      parsePattern input `shouldBe` Right (PatArray [PatLitInt 1, PatLitInt 2, PatLitInt 3])

    it "parses array pattern with variables and literals" $ do
      let input = "[x, y, 42]"
      parsePattern input `shouldBe` Right (PatArray [PatVar "x", PatVar "y", PatLitInt 42])

    it "parses exclusive range pattern" $ do
      let input = "1..10"
      parsePattern input `shouldBe` Right (PatRange (PatLitInt 1) (PatLitInt 10))

    it "parses inclusive range pattern" $ do
      let input = "1..=10"
      parsePattern input `shouldBe` Right (PatRangeInclusive (PatLitInt 1) (PatLitInt 10))

    it "parses OR pattern with literals" $ do
      let input = "1 | 2 | 3"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 1, PatLitInt 2, PatLitInt 3])

    it "parses OR pattern with variables and literals" $ do
      let input = "x | 42 | _"
      parsePattern input `shouldBe` Right (PatOr [PatVar "x", PatLitInt 42, PatWildcard])

    it "parses complex tuple pattern with nested patterns" $ do
      let input = "(x, (1, 2), [true, false])"
      parsePattern input `shouldBe` Right (PatTuple [PatVar "x", PatTuple [PatLitInt 1, PatLitInt 2], PatArray [PatLitBool True, PatLitBool False]])

    it "parses complex array pattern with nested patterns" $ do
      let input = "[x, (1, 2), [true, false]]"
      parsePattern input `shouldBe` Right (PatArray [PatVar "x", PatTuple [PatLitInt 1, PatLitInt 2], PatArray [PatLitBool True, PatLitBool False]])

    it "parses nested OR patterns" $ do
      let input = "(x | y, 42 | 100)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatVar "x", PatVar "y"], PatOr [PatLitInt 42, PatLitInt 100]])

    it "parses OR pattern with tuples" $ do
      let input = "(1, 2) | (3, 4)"
      parsePattern input `shouldBe` Right (PatOr [PatTuple [PatLitInt 1, PatLitInt 2], PatTuple [PatLitInt 3, PatLitInt 4]])

    it "parses tuple with range patterns" $ do
      let input = "(1..10, x)"
      parsePattern input `shouldBe` Right (PatTuple [PatRange (PatLitInt 1) (PatLitInt 10), PatVar "x"])

    it "parses array with range patterns" $ do
      let input = "[1..=5, 10]"
      parsePattern input `shouldBe` Right (PatArray [PatRangeInclusive (PatLitInt 1) (PatLitInt 5), PatLitInt 10])

    -- Nested tuple patterns
    it "parses deeply nested tuple pattern" $ do
      let input = "((1, 2), (3, (4, 5)))"
      parsePattern input `shouldBe` Right (PatTuple [PatTuple [PatLitInt 1, PatLitInt 2], PatTuple [PatLitInt 3, PatTuple [PatLitInt 4, PatLitInt 5]]])

    -- OR pattern with mixed literals and variables
    it "parses OR pattern with mixed types" $ do
      let input = "true | 42 | \"hello\""
      parsePattern input `shouldBe` Right (PatOr [PatLitBool True, PatLitInt 42, PatLitString "hello"])

    -- OR pattern with nested tuple and array
    it "parses OR pattern with nested tuple and array" $ do
      let input = "(1, [x, 2]) | (true, [_, false])"
      parsePattern input `shouldBe` Right (PatOr [PatTuple [PatLitInt 1, PatArray [PatVar "x", PatLitInt 2]], PatTuple [PatLitBool True, PatArray [PatWildcard, PatLitBool False]]])

    -- Wildcard with OR
    it "parses OR pattern with wildcard" $ do
      let input = "42 | _ | x"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 42, PatWildcard, PatVar "x"])

    -- Empty array
    it "parses empty array pattern" $ do
      let input = "[]"
      parsePattern input `shouldBe` Right (PatArray [])

    -- Range with variable
    it "parses range pattern with variable" $ do
      let input = "x..y"
      parsePattern input `shouldBe` Right (PatRange (PatVar "x") (PatVar "y"))

    -- Inclusive range with variable
    it "parses inclusive range pattern with variable" $ do
      let input = "x..=y"
      parsePattern input `shouldBe` Right (PatRangeInclusive (PatVar "x") (PatVar "y"))

    -- OR with range and literals
    it "parses OR pattern with range and literals" $ do
      let input = "1..5 | 10 | 15..20"
      parsePattern input `shouldBe` Right (PatOr [PatRange (PatLitInt 1) (PatLitInt 5), PatLitInt 10, PatRange (PatLitInt 15) (PatLitInt 20)])

    -- OR with tuple and range
    it "parses OR pattern with tuple and range" $ do
      let input = "(x, 1..5) | (y, 10..20)"
      parsePattern input `shouldBe` Right (PatOr [PatTuple [PatVar "x", PatRange (PatLitInt 1) (PatLitInt 5)], PatTuple [PatVar "y", PatRange (PatLitInt 10) (PatLitInt 20)]])

    -- Array with nested OR patterns
    it "parses array with OR patterns" $ do
      let input = "[1 | 2, x | y, true | false]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatLitInt 1, PatLitInt 2], PatOr [PatVar "x", PatVar "y"], PatOr [PatLitBool True, PatLitBool False]])

    -- OR with exclusive and inclusive range
    it "parses OR pattern with exclusive and inclusive range" $ do
      let input = "1..5 | 6..=10"
      parsePattern input `shouldBe` Right (PatOr [PatRange (PatLitInt 1) (PatLitInt 5), PatRangeInclusive (PatLitInt 6) (PatLitInt 10)])

    -- Complex nested OR pattern
    it "parses complex OR pattern with nested arrays and tuples" $ do
      let input = "[1, (x | y, 2)] | [true, (3, 4 | 5)]"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatLitInt 1, PatTuple [PatOr [PatVar "x", PatVar "y"], PatLitInt 2]], PatArray [PatLitBool True, PatTuple [PatLitInt 3, PatOr [PatLitInt 4, PatLitInt 5]]]])

    -- Multiple levels of OR pattern nesting
    it "parses deeply nested OR pattern" $ do
      let input = "1 | 2 | 3 | 4 | 5"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 1, PatLitInt 2, PatLitInt 3, PatLitInt 4, PatLitInt 5])

    -- Complex pattern mixing everything
    it "parses complex pattern mixing everything" $ do
     let input = "(1 | 2, [x..=10, true | false], (3..5 | 6, \"hello\"))"
     parsePattern input `shouldBe` Right (PatTuple
      [ PatOr [PatLitInt 1, PatLitInt 2]
      , PatArray [PatRangeInclusive (PatVar "x") (PatLitInt 10), PatOr [PatLitBool True, PatLitBool False]]
      , PatTuple [PatOr [PatRange (PatLitInt 3) (PatLitInt 5), PatLitInt 6], PatLitString "hello"]
      ])

    -- OR pattern with a mix of ranges, tuples and literals
    it "parses OR pattern with mix of ranges, tuples and literals" $ do
      let input = "1..5 | (x, true) | 42"
      parsePattern input `shouldBe` Right (PatOr [PatRange (PatLitInt 1) (PatLitInt 5), PatTuple [PatVar "x", PatLitBool True], PatLitInt 42])

    -- Array with single element
    it "parses array with a single element" $ do
      let input = "[42]"
      parsePattern input `shouldBe` Right (PatArray [PatLitInt 42])

    -- OR with single element
    it "parses OR pattern with a single element" $ do
      let input = "42 | _"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 42, PatWildcard])

    -- Tuple with nested inclusive range
    it "parses tuple with nested inclusive range" $ do
      let input = "(1..=5, x..=10)"
      parsePattern input `shouldBe` Right (PatTuple
        [ PatRangeInclusive (PatLitInt 1) (PatLitInt 5)
        , PatRangeInclusive (PatVar "x") (PatLitInt 10)
        ])

    -- Array with inclusive range
    it "parses array with inclusive range" $ do
      let input = "[1..=10, 42]"
      parsePattern input `shouldBe` Right (PatArray [PatRangeInclusive (PatLitInt 1) (PatLitInt 10), PatLitInt 42])

    -- OR pattern with multiple variables
    it "parses OR pattern with multiple variables" $ do
      let input = "x | y | z"
      parsePattern input `shouldBe` Right (PatOr [PatVar "x", PatVar "y", PatVar "z"])

    -- Complex pattern with a mix of everything
    it "parses highly complex pattern" $ do
      let input = "(x, [1..5, true], (y | 42))"
      parsePattern input `shouldBe` Right (PatTuple [PatVar "x", PatArray [PatRange (PatLitInt 1) (PatLitInt 5), PatLitBool True], PatTuple [PatOr [PatVar "y", PatLitInt 42]]])

    -- Single variable OR literal
    it "parses OR pattern with variable and literal" $ do
      let input = "x | 10"
      parsePattern input `shouldBe` Right (PatOr [PatVar "x", PatLitInt 10])

    -- Single variable OR boolean
    it "parses OR pattern with variable and boolean" $ do
      let input = "x | true"
      parsePattern input `shouldBe` Right (PatOr [PatVar "x", PatLitBool True])

    -- Single literal OR boolean
    it "parses OR pattern with literal and boolean" $ do
      let input = "10 | false"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 10, PatLitBool False])

    -- Single variable OR wildcard
    it "parses OR pattern with variable and wildcard" $ do
      let input = "x | _"
      parsePattern input `shouldBe` Right (PatOr [PatVar "x", PatWildcard])

    -- Array with mixed literals and variables
    it "parses array pattern with mixed literals and variables" $ do
      let input = "[x, true, 42]"
      parsePattern input `shouldBe` Right (PatArray [PatVar "x", PatLitBool True, PatLitInt 42])

    -- Array with wildcard and boolean
    it "parses array pattern with wildcard and boolean" $ do
      let input = "[_, true]"
      parsePattern input `shouldBe` Right (PatArray [PatWildcard, PatLitBool True])

    -- OR pattern with arrays
    it "parses OR pattern with arrays" $ do
      let input = "[1, 2] | [3, 4]"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatLitInt 1, PatLitInt 2], PatArray [PatLitInt 3, PatLitInt 4]])

    -- Array with nested OR
    it "parses array with nested OR patterns" $ do
      let input = "[x | y, 1 | 2]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatVar "y"], PatOr [PatLitInt 1, PatLitInt 2]])

    -- Array with empty array
    it "parses array containing empty array" $ do
      let input = "[[]]"
      parsePattern input `shouldBe` Right (PatArray [PatArray []])

    -- Array with multiple empty arrays
    it "parses array containing multiple empty arrays" $ do
      let input = "[[], []]"
      parsePattern input `shouldBe` Right (PatArray [PatArray [], PatArray []])

    -- OR with array and literal
    it "parses OR pattern with array and literal" $ do
      let input = "[x, y] | 42"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatVar "x", PatVar "y"], PatLitInt 42])

    -- OR with array and wildcard
    it "parses OR pattern with array and wildcard" $ do
      let input = "[x, y] | _"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatVar "x", PatVar "y"], PatWildcard])

    -- OR with range and array
    it "parses OR pattern with range and array" $ do
      let input = "1..5 | [true, false]"
      parsePattern input `shouldBe` Right (PatOr [PatRange (PatLitInt 1) (PatLitInt 5), PatArray [PatLitBool True, PatLitBool False]])

    -- Array with OR and range
    it "parses array with OR pattern and range" $ do
      let input = "[x | y, 1..5]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatVar "y"], PatRange (PatLitInt 1) (PatLitInt 5)])

    -- Nested OR pattern with variables and ranges
    it "parses nested OR pattern with variables and ranges" $ do
      let input = "(x..5 | y..10)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatRange (PatVar "x") (PatLitInt 5), PatRange (PatVar "y") (PatLitInt 10)]])

    -- Array with multiple nested OR patterns
    it "parses array with multiple nested OR patterns" $ do
      let input = "[x | y, 1 | 2, true | false]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatVar "y"], PatOr [PatLitInt 1, PatLitInt 2], PatOr [PatLitBool True, PatLitBool False]])

    -- Array with OR and nested arrays
    it "parses array with OR pattern and nested arrays" $ do
      let input = "[[1, 2] | [3, 4]]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatArray [PatLitInt 1, PatLitInt 2], PatArray [PatLitInt 3, PatLitInt 4]]])

    -- Array with OR and wildcard
    it "parses array with OR pattern and wildcard" $ do
      let input = "[_, x | y]"
      parsePattern input `shouldBe` Right (PatArray [PatWildcard, PatOr [PatVar "x", PatVar "y"]])

    -- Empty range in OR
    it "parses OR pattern with empty array and range" $ do
      let input = "[] | 1..5"
      parsePattern input `shouldBe` Right (PatOr [PatArray [], PatRange (PatLitInt 1) (PatLitInt 5)])

    -- Wildcard with nested array
    it "parses OR pattern with wildcard and nested array" $ do
      let input = "_ | [x, y]"
      parsePattern input `shouldBe` Right (PatOr [PatWildcard, PatArray [PatVar "x", PatVar "y"]])

    -- Range with OR and boolean
    it "parses OR pattern with range and boolean" $ do
      let input = "1..5 | true"
      parsePattern input `shouldBe` Right (PatOr [PatRange (PatLitInt 1) (PatLitInt 5), PatLitBool True])

    -- Array with boolean and literal
    it "parses array with boolean and literal" $ do
      let input = "[true, 42]"
      parsePattern input `shouldBe` Right (PatArray [PatLitBool True, PatLitInt 42])

    -- Range with boolean and array
    it "parses OR pattern with range, boolean and array" $ do
      let input = "1..5 | true | [false, 42]"
      parsePattern input `shouldBe` Right (PatOr [PatRange (PatLitInt 1) (PatLitInt 5), PatLitBool True, PatArray [PatLitBool False, PatLitInt 42]])

    -- Array with OR pattern and boolean
    it "parses array with OR pattern and boolean" $ do
      let input = "[x | y, true]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatVar "y"], PatLitBool True])

    -- Nested array with range
    it "parses nested array with range" $ do
      let input = "[[1..5, 42]]"
      parsePattern input `shouldBe` Right (PatArray [PatArray [PatRange (PatLitInt 1) (PatLitInt 5), PatLitInt 42]])

    -- Nested array with wildcard
    it "parses nested array with wildcard" $ do
      let input = "[[_, 42]]"
      parsePattern input `shouldBe` Right (PatArray [PatArray [PatWildcard, PatLitInt 42]])

    -- Array with nested OR and wildcard
    it "parses array with nested OR and wildcard" $ do
      let input = "[x | _, true]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatWildcard], PatLitBool True])

    -- Array with inclusive range and boolean
    it "parses array with inclusive range and boolean" $ do
      let input = "[1..=10, true]"
      parsePattern input `shouldBe` Right (PatArray [PatRangeInclusive (PatLitInt 1) (PatLitInt 10), PatLitBool True])

    -- OR pattern with inclusive range and literal
    it "parses OR pattern with inclusive range and literal" $ do
      let input = "1..=10 | 42"
      parsePattern input `shouldBe` Right (PatOr [PatRangeInclusive (PatLitInt 1) (PatLitInt 10), PatLitInt 42])

    -- OR pattern with inclusive range and boolean
    it "parses OR pattern with inclusive range and boolean" $ do
      let input = "1..=10 | true"
      parsePattern input `shouldBe` Right (PatOr [PatRangeInclusive (PatLitInt 1) (PatLitInt 10), PatLitBool True])

    -- Array with OR pattern and range
    it "parses array with OR pattern and range" $ do
      let input = "[x | y, 1..5]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatVar "y"], PatRange (PatLitInt 1) (PatLitInt 5)])

    -- OR pattern with array and range
    it "parses OR pattern with array and range" $ do
      let input = "[x, y] | 1..5"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatVar "x", PatVar "y"], PatRange (PatLitInt 1) (PatLitInt 5)])

    -- OR pattern with array, range and boolean
    it "parses OR pattern with array, range and boolean" $ do
      let input = "[x, y] | 1..5 | true"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatVar "x", PatVar "y"], PatRange (PatLitInt 1) (PatLitInt 5), PatLitBool True])

    -- OR pattern with boolean and wildcard
    it "parses OR pattern with boolean and wildcard" $ do
      let input = "true | _"
      parsePattern input `shouldBe` Right (PatOr [PatLitBool True, PatWildcard])

    -- OR pattern with literal and wildcard
    it "parses OR pattern with literal and wildcard" $ do
      let input = "42 | _"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 42, PatWildcard])

    -- Nested OR with literal and boolean
    it "parses nested OR pattern with literal and boolean" $ do
      let input = "(42 | true)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatLitInt 42,PatLitBool True]])

    -- Nested OR with variable and literal
    it "parses nested OR pattern with variable and literal" $ do
      let input = "(x | 42)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatVar "x", PatLitInt 42]])

    -- Nested OR with boolean and variable
    it "parses nested OR pattern with boolean and variable" $ do
      let input = "(true | x)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatLitBool True, PatVar "x"]])

    -- OR pattern with multiple literals and wildcard
    it "parses OR pattern with multiple literals and wildcard" $ do
      let input = "1 | 2 | _"
      parsePattern input `shouldBe` Right (PatOr [PatLitInt 1, PatLitInt 2, PatWildcard])

    -- Nested OR with multiple literals and variable
    it "parses nested OR pattern with multiple literals and variable" $ do
      let input = "(1 | 2 | x)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatLitInt 1, PatLitInt 2, PatVar "x"]])

    -- Array with OR and boolean
    it "parses array with OR and boolean" $ do
      let input = "[x | true, false]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatLitBool True], PatLitBool False])

    -- Array with OR and inclusive range
    it "parses array with OR and inclusive range" $ do
      let input = "[1 | 2, 3..=5]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatLitInt 1, PatLitInt 2], PatRangeInclusive (PatLitInt 3) (PatLitInt 5)])

    -- OR pattern with nested arrays and boolean
    it "parses OR pattern with nested arrays and boolean" $ do
      let input = "[1, 2] | [true, false]"
      parsePattern input `shouldBe` Right (PatOr [PatArray [PatLitInt 1, PatLitInt 2], PatArray [PatLitBool True, PatLitBool False]])

    -- Array with OR and variable
    it "parses array with OR and variable" $ do
      let input = "[x | y, 42]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatVar "x", PatVar "y"], PatLitInt 42])

    -- Array with OR and literal
    it "parses array with OR and literal" $ do
      let input = "[1 | 2, 3]"
      parsePattern input `shouldBe` Right (PatArray [PatOr [PatLitInt 1, PatLitInt 2], PatLitInt 3])

    it "parses nested OR pattern with inclusive range" $ do
      let input = "(1..=5 | 6..=10)"
      parsePattern input `shouldBe` Right (PatTuple [PatOr [PatRangeInclusive (PatLitInt 1) (PatLitInt 5), PatRangeInclusive (PatLitInt 6) (PatLitInt 10)]])

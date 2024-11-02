module TypeSpec (spec) where

import Test.Hspec
import Parser.Type (parseType, Type(..))

spec :: Spec
spec = do
  describe "Type parsing" $ do
    
    it "parses int type" $ do
      let input = "int"
      parseType input `shouldBe` Right TInt

    it "parses string type" $ do
      let input = "string"
      parseType input `shouldBe` Right TString

    it "parses bool type" $ do
      let input = "bool"
      parseType input `shouldBe` Right TBool

    it "parses void type" $ do
      let input = "void"
      parseType input `shouldBe` Right TVoid

    it "parses array type" $ do
      let input = "int[]"
      parseType input `shouldBe` Right (TArray TInt)

    it "parses tuple type" $ do
      let input = "(int, string)"
      parseType input `shouldBe` Right (TTuple [TInt, TString])

    it "fails to parse function type with one parameter without parentheses" $ do
      let input = "int -> string"
      case parseType input of
        Left _ -> return ()
        Right result -> expectationFailure $ "Expected parsing failure, but got success." ++ show result

    it "parses function type with single parameter" $ do
      let input = "(int) -> string"
      parseType input `shouldBe` Right (TFunc [TInt] TString)

    it "fails to parse function type with multiple parameters without parentheses" $ do
      let input = "int, string -> bool"
      case parseType input of
        Left _ -> return ()
        Right result -> expectationFailure $ "Expected parsing failure, but got success." ++ show result

    it "fails to parse nested function type" $ do
      let input = "int -> (string -> bool)"
      case parseType input of
        Left _ -> return ()
        Right result -> expectationFailure $ "Expected parsing failure, but got success." ++ show result

    it "parses unknown type" $ do
      let input = "customType"
      parseType input `shouldBe` Right (TUnknown "customType")

    it "parses array of tuples" $ do
      let input = "(int, string)[]"
      parseType input `shouldBe` Right (TArray (TTuple [TInt, TString]))

    it "parses array of function types" $ do
      let input = "((int) -> bool)[]"
      parseType input `shouldBe` Right (TArray (TFunc [TInt] TBool))

    it "parses nested arrays" $ do
      let input = "int[][]"
      parseType input `shouldBe` Right (TArray (TArray TInt))

    it "parses function type returning an array" $ do
      let input = "(int) -> string[]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray TString))

    it "parses function type with tuple as parameter" $ do
      let input = "((int, string)) -> bool"
      parseType input `shouldBe` Right (TFunc [TTuple [TInt, TString]] TBool)

    it "parses function type with multiple parameters" $ do
      let input = "(int, string) -> bool"
      parseType input `shouldBe` Right (TFunc [TInt, TString] TBool)

    it "parses deeply nested arrays" $ do
      let input = "int[][][]"
      parseType input `shouldBe` Right (TArray (TArray (TArray TInt)))

    it "parses array of functions returning tuples" $ do
      let input = "((int) -> (string, bool))[]"
      parseType input `shouldBe` Right (TArray (TFunc [TInt] (TTuple [TString, TBool])))

    it "parses function returning array of arrays" $ do
      let input = "(int) -> int[][]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray (TArray TInt)))

    it "parses array of function returning another array" $ do
      let input = "((int) -> int[])[]"
      parseType input `shouldBe` Right (TArray (TFunc [TInt] (TArray TInt)))

    it "parses tuple of arrays and functions" $ do
      let input = "(int[], (int) -> bool)"
      parseType input `shouldBe` Right (TTuple [TArray TInt, TFunc [TInt] TBool])

    it "parses nested tuple with arrays" $ do
      let input = "((int, string[]), bool)"
      parseType input `shouldBe` Right (TTuple [TTuple [TInt, TArray TString], TBool])

    it "parses function with array as parameter" $ do
      let input = "(int[]) -> bool"
      parseType input `shouldBe` Right (TFunc [TArray TInt] TBool)

    it "parses tuple of arrays" $ do
      let input = "(int[], string[])"
      parseType input `shouldBe` Right (TTuple [TArray TInt, TArray TString])

    it "parses function returning tuple of arrays" $ do
      let input = "(int) -> (int[], string[])"
      parseType input `shouldBe` Right (TFunc [TInt] (TTuple [TArray TInt, TArray TString]))

    it "parses tuple of function types" $ do
      let input = "((int) -> string, (bool) -> int)"
      parseType input `shouldBe` Right (TTuple [TFunc [TInt] TString, TFunc [TBool] TInt])

    it "parses tuple of arrays of functions" $ do
      let input = "(((int) -> string)[], ((bool) -> int)[])"
      parseType input `shouldBe` Right (TTuple [TArray (TFunc [TInt] TString), TArray (TFunc [TBool] TInt)])

    it "parses function with tuple parameter" $ do
      let input = "((int, string)) -> bool"
      parseType input `shouldBe` Right (TFunc [TTuple [TInt, TString]] TBool)

    it "parses function returning tuple with function" $ do
      let input = "(int) -> ((string) -> bool, int)"
      parseType input `shouldBe` Right (TFunc [TInt] (TTuple [TFunc [TString] TBool, TInt]))

    it "parses function returning tuple of arrays and functions" $ do
      let input = "(int) -> (int[], (string) -> bool)"
      parseType input `shouldBe` Right (TFunc [TInt] (TTuple [TArray TInt, TFunc [TString] TBool]))

    it "parses deeply nested tuples" $ do
      let input = "((int, (string, bool)), (float, (char)))"
      parseType input `shouldBe` Right (TTuple [TTuple [TInt, TTuple [TString, TBool]], TTuple [TUnknown "float", TUnknown "char"]])

    it "parses array of tuples" $ do
      let input = "(int, string)[]"
      parseType input `shouldBe` Right (TArray (TTuple [TInt, TString]))

    it "parses function returning array of tuples" $ do
      let input = "(int) -> (int, string)[]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray (TTuple [TInt, TString])))

    it "parses tuple with function returning arrays" $ do
      let input = "((int) -> string[], (bool) -> int[])"
      parseType input `shouldBe` Right (TTuple [TFunc [TInt] (TArray TString), TFunc [TBool] (TArray TInt)])

    it "parses function with tuple and array as parameters" $ do
      let input = "((int, string[]), bool) -> int"
      parseType input `shouldBe` Right (TFunc [TTuple [TInt, TArray TString], TBool] TInt)

    it "parses array of function returning tuples" $ do
      let input = "((int) -> (string, bool))[]"
      parseType input `shouldBe` Right (TArray (TFunc [TInt] (TTuple [TString, TBool])))

    it "parses function with complex tuple parameter" $ do
      let input = "((int, (string, bool))) -> int"
      parseType input `shouldBe` Right (TFunc [TTuple [TInt, TTuple [TString, TBool]]] TInt)

    it "parses function returning complex tuple" $ do
      let input = "(int) -> ((string, bool), (int, float))"
      parseType input `shouldBe` Right (TFunc [TInt] (TTuple [TTuple [TString, TBool], TTuple [TInt, TUnknown "float"]]))

    it "parses array of complex tuples" $ do
      let input = "((int, string[]), (bool, float))[]"
      parseType input `shouldBe` Right (TArray (TTuple [TTuple [TInt, TArray TString], TTuple [TBool, TUnknown "float"]]))

    it "parses array of function types returning complex tuples" $ do
      let input = "((int) -> (string, bool))[]"
      parseType input `shouldBe` Right (TArray (TFunc [TInt] (TTuple [TString, TBool])))

    it "parses tuple of arrays of function types" $ do
      let input = "(((int) -> string)[], ((bool) -> int)[])"
      parseType input `shouldBe` Right (TTuple [TArray (TFunc [TInt] TString), TArray (TFunc [TBool] TInt)])

    it "parses function returning tuple with nested function" $ do
      let input = "(int) -> ((string) -> bool, (int) -> string)"
      parseType input `shouldBe` Right (TFunc [TInt] (TTuple [TFunc [TString] TBool, TFunc [TInt] TString]))

    it "parses complex function returning array of tuples" $ do
      let input = "(int) -> ((string, bool), (int, float))[]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray (TTuple [TTuple [TString, TBool], TTuple [TInt, TUnknown "float"]])))

    it "parses array of nested function types" $ do
      let input = "(((int) -> (string) -> bool)[])"
      parseType input `shouldBe` Right (TArray (TFunc [TInt] (TFunc [TString] TBool)))

    it "parses function with array and tuple parameters" $ do
      let input = "(int[], (string, bool)) -> float"
      parseType input `shouldBe` Right (TFunc [TArray TInt, TTuple [TString, TBool]] (TUnknown "float"))

    it "parses function returning complex array of functions" $ do
      let input = "(int) -> ((string) -> bool)[]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray (TFunc [TString] TBool)))

    it "parses deeply nested array of functions" $ do
      let input = "(((int) -> (string)) -> bool)[]"
      parseType input `shouldBe` Right (TArray (TFunc [TFunc [TInt] TString] TBool))

    it "parses deeply nested tuples of arrays" $ do
      let input = "(((int, string[]), bool), float[])"
      parseType input `shouldBe` Right (TTuple [TTuple [TTuple [TInt, TArray TString], TBool], TArray (TUnknown "float")])

    it "parses function returning array of deeply nested tuples" $ do
      let input = "(int) -> (((string, bool), int), float)[]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray (TTuple [TTuple [TTuple [TString, TBool], TInt], TUnknown "float"])))

    it "parses array of tuples with nested arrays" $ do
      let input = "((int[], string[]), bool[])[]"
      parseType input `shouldBe` Right (TArray (TTuple [TTuple [TArray TInt, TArray TString], TArray TBool]))

    it "parses function returning array of nested tuples with arrays" $ do
      let input = "(int) -> ((string[], bool[]), float[])[]"
      parseType input `shouldBe` Right (TFunc [TInt] (TArray (TTuple [TTuple [TArray TString, TArray TBool], TArray (TUnknown "float")])))

    it "parses complex array of tuples with functions" $ do
      let input = "((int[], (string) -> bool), ((int) -> string, bool))[]"
      parseType input `shouldBe` Right (TArray (TTuple [TTuple [TArray TInt, TFunc [TString] TBool], TTuple [TFunc [TInt] TString, TBool]]))

    it "parses tuple with mixed types, including functions and arrays" $ do
      let input = "(int, (string) -> bool, bool[], float)"
      parseType input `shouldBe` Right (TTuple [TInt, TFunc [TString] TBool, TArray TBool, TUnknown "float"])

    it "parses deeply nested functions and tuples" $ do
      let input = "((int) -> ((string) -> bool), (float) -> (char))"
      parseType input `shouldBe` Right (TTuple [TFunc [TInt] (TFunc [TString] TBool), TFunc [TUnknown "float"] (TUnknown "char")])

    it "parses array of deeply nested functions and tuples" $ do
      let input = "(((int) -> ((string) -> bool), (float) -> (char)))[]"
      parseType input `shouldBe` Right (TArray (TTuple [TFunc [TInt] (TFunc [TString] TBool), TFunc [TUnknown "float"] (TUnknown "char")]))

    it "parses function with tuple containing arrays and functions" $ do
      let input = "((int[], (string) -> bool), (float) -> (char)) -> bool"
      parseType input `shouldBe` Right (TFunc [TTuple [TArray TInt, TFunc [TString] TBool], TFunc [TUnknown "float"] (TUnknown "char")] TBool)

    it "parses function with tuple containing one array and a function" $ do
      let input = "(int[], (string) -> bool) -> bool"
      parseType input `shouldBe` Right (TFunc [TArray TInt, TFunc [TString] TBool] TBool)

    it "parses function with tuple containing a function and an array" $ do
      let input = "((int) -> bool, bool[]) -> bool"
      parseType input `shouldBe` Right (TFunc [TFunc [TInt] TBool, TArray TBool] TBool)

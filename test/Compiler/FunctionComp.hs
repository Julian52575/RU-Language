module Compiler.FunctionComp (
    spec
) where

import Test.Hspec
import Compiler.Function (getFunctionTable)
import Parser.AST (Stmt(..))
import Data.Maybe (fromJust)
import Data.List (elemIndex, find)
import Compiler.Type (OpCode(..), Function(..), Scope(..), Compile(..))
import Parser.Type (Type(..))

spec :: Spec
spec = do
  describe "Function Table" $ do

    it "returns an empty list for an empty input" $
      let input = []
          strTable = []
      in getFunctionTable input strTable `shouldBe` []

    it "returns a list with one function for a single function input" $
      let input = [FuncDeclStmt "f" [] TInt Nothing]
          strTable = ["f"]
          expected = [Function 0 "f" Nothing Nothing]
      in getFunctionTable input strTable `shouldBe` expected

    it "returns a list with multiple functions for multiple function inputs" $
      let input = [FuncDeclStmt "f" [] TInt Nothing, FuncDeclStmt "g" [] TInt Nothing]
          strTable = ["f", "g"]
          expected = [Function 0 "f" Nothing Nothing, Function 1 "g" Nothing Nothing]
      in getFunctionTable input strTable `shouldBe` expected

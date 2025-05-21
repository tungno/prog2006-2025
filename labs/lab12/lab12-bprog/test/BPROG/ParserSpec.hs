module BPROG.ParserSpec (spec) where

import Test.Hspec
import BPROG.Parser
import BPROG.Types
import BPROG.Error
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "BPROG.Parser" $ do
  describe "parseTokens" $ do
    it "parses integer literals" $ do
      result <- runExceptT $ parseTokens [T.pack "42"]
      case result of
        Right [IntVal 42] -> return ()
        _ -> expectationFailure $ "Expected [IntVal 42], got " ++ show result

    it "parses float literals" $ do
      result <- runExceptT $ parseTokens [T.pack "3.14"]
      case result of
        Right [FloatVal 3.14] -> return ()
        _ -> expectationFailure $ "Expected [FloatVal 3.14], got " ++ show result

    it "parses boolean literals" $ do
      result <- runExceptT $ parseTokens [T.pack "True", T.pack "False"]
      case result of
        Right [BoolVal True, BoolVal False] -> return ()
        _ -> expectationFailure $ "Expected [BoolVal True, BoolVal False], got " ++ show result

    it "parses string literals" $ do
      result <- runExceptT $ parseTokens [T.pack "\"hello\""]
      case result of
        Right [StringVal s] -> T.unpack s `shouldBe` "hello"
        _ -> expectationFailure $ "Expected [StringVal \"hello\"], got " ++ show result

    it "parses symbols" $ do
      result <- runExceptT $ parseTokens [T.pack "foo"]
      case result of
        Right [SymbolVal s] -> T.unpack s `shouldBe` "foo"
        _ -> expectationFailure $ "Expected [SymbolVal \"foo\"], got " ++ show result

  describe "parseLine" $ do
    it "parses a simple line" $ do
      result <- runExceptT $ parseLine (T.pack "1 2 +")
      case result of
        Right tokens -> length tokens `shouldBe` 3
        Left err -> expectationFailure $ "Parser error: " ++ show err

  describe "parseProgram" $ do
    it "parses a simple program" $ do
      result <- runExceptT $ parseProgram (T.pack "1 2 +\n3 4 *")
      case result of
        Right tokens -> length tokens `shouldBe` 6
        Left err -> expectationFailure $ "Parser error: " ++ show err
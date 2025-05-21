module BPROG.OperationsSpec (spec) where

import Test.Hspec
import BPROG.Types
import BPROG.Error
import BPROG.Operations.Arithmetic as Arithmetic
import BPROG.Operations.Stack as Stack
import BPROG.Operations.Logic as Logic
import BPROG.Operations.Comparison as Comparison
import BPROG.Operations.List as List
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "BPROG.Operations" $ do
  describe "Arithmetic operations" $ do
    it "adds two integers" $ do
      let state = InterpreterState { stack = [IntVal 2, IntVal 3], symDict = [] }
      result <- runExceptT $ Arithmetic.add state
      case result of
        Right newState -> 
          case stack newState of
            [IntVal 5] -> return ()
            _ -> expectationFailure $ "Expected [IntVal 5], got " ++ show (stack newState)
        Left err -> expectationFailure $ "Operation error: " ++ show err

    it "multiplies two integers" $ do
      let state = InterpreterState { stack = [IntVal 2, IntVal 3], symDict = [] }
      result <- runExceptT $ Arithmetic.multiply state
      case result of
        Right newState -> 
          case stack newState of
            [IntVal 6] -> return ()
            _ -> expectationFailure $ "Expected [IntVal 6], got " ++ show (stack newState)
        Left err -> expectationFailure $ "Operation error: " ++ show err

  describe "Stack operations" $ do
    it "duplicates a value" $ do
      let state = InterpreterState { stack = [IntVal 5], symDict = [] }
      result <- runExceptT $ Stack.dup state
      case result of
        Right newState -> 
          case stack newState of
            [IntVal 5, IntVal 5] -> return ()
            _ -> expectationFailure $ "Expected [IntVal 5, IntVal 5], got " ++ show (stack newState)
        Left err -> expectationFailure $ "Operation error: " ++ show err

    it "swaps two values" $ do
      let state = InterpreterState { stack = [IntVal 1, IntVal 2], symDict = [] }
      result <- runExceptT $ Stack.swap state
      case result of
        Right newState -> 
          case stack newState of
            [IntVal 2, IntVal 1] -> return ()
            _ -> expectationFailure $ "Expected [IntVal 2, IntVal 1], got " ++ show (stack newState)
        Left err -> expectationFailure $ "Operation error: " ++ show err

  describe "Logic operations" $ do
    it "performs logical AND" $ do
      let state = InterpreterState { stack = [BoolVal True, BoolVal False], symDict = [] }
      result <- runExceptT $ Logic.and state
      case result of
        Right newState -> 
          case stack newState of
            [BoolVal False] -> return ()
            _ -> expectationFailure $ "Expected [BoolVal False], got " ++ show (stack newState)
        Left err -> expectationFailure $ "Operation error: " ++ show err
module BPROG.Operations.Comparison
  ( -- * Comparison operations
    lessThan
  , greaterThan
  , equals
  ) where

import BPROG.Types
import BPROG.Error
import Data.Text (Text)
import qualified Data.Text as T

-- | Less than operation (x y -- x<y)
lessThan :: InterpreterState -> Interpreter InterpreterState
lessThan state = case stack state of
  (val2:val1:rest) -> do
    result <- lessThanValues val1 val2
    return state { stack = BoolVal result : rest }
  _ -> throwError StackEmpty

-- | Greater than operation (x y -- x>y)
greaterThan :: InterpreterState -> Interpreter InterpreterState
greaterThan state = case stack state of
  (val2:val1:rest) -> do
    result <- greaterThanValues val1 val2
    return state { stack = BoolVal result : rest }
  (val:_) -> throwError ExpectedNumber
  [] -> throwError StackEmpty

-- | Equality operation (x y -- x==y)
equals :: InterpreterState -> Interpreter InterpreterState
equals state = case stack state of
  (val2:val1:rest) -> do
    result <- equalsValues val1 val2
    return state { stack = BoolVal result : rest }
  _ -> throwError StackEmpty

-- Helper functions for comparison operations

-- | Compare two values for less than
lessThanValues :: Value -> Value -> Interpreter Bool
lessThanValues (IntVal a) (IntVal b) = return (a < b)
lessThanValues (FloatVal a) (FloatVal b) = return (a < b)
lessThanValues (IntVal a) (FloatVal b) = return (fromIntegral a < b)
lessThanValues (FloatVal a) (IntVal b) = return (a < fromIntegral b)
lessThanValues (StringVal a) (StringVal b) = return (a < b)
lessThanValues _ _ = throwError ExpectedNumber

-- | Compare two values for greater than
greaterThanValues :: Value -> Value -> Interpreter Bool
greaterThanValues (IntVal a) (IntVal b) = return (a > b)
greaterThanValues (FloatVal a) (FloatVal b) = return (a > b)
greaterThanValues (IntVal a) (FloatVal b) = return (fromIntegral a > b)
greaterThanValues (FloatVal a) (IntVal b) = return (a > fromIntegral b)
greaterThanValues (StringVal a) (StringVal b) = return (a > b)
greaterThanValues _ _ = throwError ExpectedNumber

-- | Compare two values for equality
equalsValues :: Value -> Value -> Interpreter Bool
equalsValues (IntVal a) (IntVal b) = return (a == b)
equalsValues (FloatVal a) (FloatVal b) = return (a == b)
equalsValues (IntVal a) (FloatVal b) = return (fromIntegral a == b)
equalsValues (FloatVal a) (IntVal b) = return (a == fromIntegral b)
equalsValues (BoolVal a) (BoolVal b) = return (a == b)
equalsValues (StringVal a) (StringVal b) = return (a == b)
equalsValues (ListVal a) (ListVal b) = return (a == b)
equalsValues (QuotationVal a) (QuotationVal b) = return (a == b)
equalsValues (SymbolVal a) (SymbolVal b) = return (a == b)
equalsValues _ _ = return False  -- Different types are never equal
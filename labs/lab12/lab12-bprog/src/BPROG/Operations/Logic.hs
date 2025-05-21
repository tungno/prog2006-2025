module BPROG.Operations.Logic
  ( -- * Logic operations
    and
  , or
  , not
  ) where

import Prelude hiding (and, or, not)
import qualified Prelude
import BPROG.Types
import BPROG.Error

-- | Logical AND operation (x y -- x&&y)
and :: InterpreterState -> Interpreter InterpreterState
and state = case stack state of
  (val2:val1:rest) -> do
    result <- andValues val1 val2
    return state { stack = result : rest }
  _ -> throwError StackEmpty

-- | Logical OR operation (x y -- x||y)
or :: InterpreterState -> Interpreter InterpreterState
or state = case stack state of
  (val2:val1:rest) -> do
    result <- orValues val1 val2
    return state { stack = result : rest }
  _ -> throwError StackEmpty

-- | Logical NOT operation (x -- !x)
not :: InterpreterState -> Interpreter InterpreterState
not state = case stack state of
  (val:rest) -> do
    result <- notValue val
    return state { stack = result : rest }
  _ -> throwError StackEmpty

-- Helper functions for logic operations

-- | AND two values
andValues :: Value -> Value -> Interpreter Value
andValues (BoolVal a) (BoolVal b) = return $ BoolVal (a && b)
andValues _ _ = throwError ExpectedBool

-- | OR two values
orValues :: Value -> Value -> Interpreter Value
orValues (BoolVal a) (BoolVal b) = return $ BoolVal (a || b)
orValues _ _ = throwError ExpectedBool

-- | NOT a value
notValue :: Value -> Interpreter Value
notValue (BoolVal a) = return $ BoolVal (Prelude.not a)
notValue (IntVal a) = return $ IntVal (-a)  -- Negation for integers
notValue _ = throwError ExpectedBoolOrNumber
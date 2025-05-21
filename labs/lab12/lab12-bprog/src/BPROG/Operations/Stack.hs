module BPROG.Operations.Stack
  ( -- * Stack operations
    dup
  , swap
  , pop
  ) where

import BPROG.Types
import BPROG.Error

-- | Duplicate the top element on the stack (x -- x x)
dup :: InterpreterState -> Interpreter InterpreterState
dup state = case stack state of
  [] -> throwError StackEmpty
  (x:xs) -> return state { stack = x : x : xs }

-- | Swap the top two elements on the stack (x y -- y x)
swap :: InterpreterState -> Interpreter InterpreterState
swap state = case stack state of
  (x:y:xs) -> return state { stack = y : x : xs }
  _ -> throwError StackEmpty

-- | Remove the top element from the stack (x --)
pop :: InterpreterState -> Interpreter InterpreterState
pop state = case stack state of
  [] -> throwError StackEmpty
  (_:xs) -> return state { stack = xs }
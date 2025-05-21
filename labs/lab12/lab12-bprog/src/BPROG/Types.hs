module BPROG.Types
  ( -- * Core types
    Value(..)
  , Stack
  , SymbolDict
  , InterpreterState(..)
  , emptyState
    -- * Interpreter monad
  , Interpreter
  , throwError
  , catchError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import Control.Monad.IO.Class (MonadIO)
import BPROG.Error
import Data.List (intercalate)

-- | BPROG values
data Value
  = IntVal Integer       -- ^ Integer value
  | FloatVal Double      -- ^ Floating point value
  | BoolVal Bool         -- ^ Boolean value
  | StringVal Text       -- ^ String value
  | ListVal [Value]      -- ^ List of values
  | QuotationVal [Text]  -- ^ Code block (stored as tokens)
  | SymbolVal Text       -- ^ Symbol (variable or function name)
  deriving (Eq)

-- | Custom show instance for Value
instance Show Value where
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (BoolVal True) = "True"
  show (BoolVal False) = "False"
  show (StringVal s) = "\"" ++ T.unpack s ++ "\""
  show (ListVal vs) = "[" ++ intercalate "," (map show vs) ++ "]"
  show (QuotationVal tokens) = "{ " ++ unwords (map T.unpack tokens) ++ " }"
  show (SymbolVal s) = T.unpack s

-- | Type alias for the operand stack
type Stack = [Value]

-- | Type alias for the symbol dictionary
type SymbolDict = [(Text, Value)]

-- | Interpreter state
data InterpreterState = InterpreterState
  { stack :: Stack         -- ^ Operand stack
  , symDict :: SymbolDict  -- ^ Symbol dictionary
  } deriving (Show)

-- | Empty initial state
emptyState :: InterpreterState
emptyState = InterpreterState
  { stack = []
  , symDict = []
  }

-- | The Interpreter monad
type Interpreter = ExceptT ProgramError IO

-- | Throw an interpreter error
throwError :: ProgramError -> Interpreter a
throwError = throwE

-- | Catch an interpreter error
catchError :: Interpreter a -> (ProgramError -> Interpreter a) -> Interpreter a
catchError = catchE
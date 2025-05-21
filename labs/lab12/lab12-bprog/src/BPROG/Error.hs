module BPROG.Error
  ( -- * Error types
    ProgramError(..)
  , ParserError(..)
  ) where

-- | Represents program execution errors
data ProgramError
  = StackEmpty
  | UnknownSymbol String
  | ExpectedBool
  | ExpectedBoolOrNumber
  | ExpectedEnumerable
  | ExpectedQuotation
  | ExpectedList
  | ExpectedString
  | ExpectedVariable
  | ExpectedNumber
  | ExpectedInteger
  | ExpectedFloat
  | DivisionByZero
  | ParseError ParserError
  | ProgramFinishedWithNoValues
  | ProgramFinishedWithMultipleValues
  | NumberConversionError
  | IOException String
  | NotImplemented String
  deriving (Eq)

-- | Custom show instance for ProgramError
instance Show ProgramError where
  show StackEmpty = "Stack underflow - not enough elements on the stack"
  show (UnknownSymbol s) = "Unknown symbol: " ++ s
  show ExpectedBool = "Expected a boolean value"
  show ExpectedBoolOrNumber = "Expected a boolean or numeric value"
  show ExpectedEnumerable = "Expected an enumerable (list or string)"
  show ExpectedQuotation = "Expected a quotation (code block)"
  show ExpectedList = "Expected a list"
  show ExpectedString = "Expected a string"
  show ExpectedVariable = "Expected a variable name (symbol)"
  show ExpectedNumber = "Expected a number (integer or float)"
  show ExpectedInteger = "Expected an integer"
  show ExpectedFloat = "Expected a float"
  show DivisionByZero = "Division by zero"
  show (ParseError err) = "Parse error: " ++ show err
  show ProgramFinishedWithNoValues = "Program finished with no values on the stack"
  show ProgramFinishedWithMultipleValues = "Program finished with multiple values on the stack"
  show NumberConversionError = "Failed to convert string to number"
  show (IOException msg) = "I/O error: " ++ msg
  show (NotImplemented feature) = "Feature not implemented: " ++ feature

-- | Represents parser errors
data ParserError
  = IncompleteString
  | IncompleteList
  | IncompleteQuotation
  | InvalidToken String
  | UnexpectedEnd
  deriving (Eq)

-- | Custom show instance for ParserError
instance Show ParserError where
  show IncompleteString = "Incomplete string literal"
  show IncompleteList = "Incomplete list literal"
  show IncompleteQuotation = "Incomplete quotation (code block)"
  show (InvalidToken t) = "Invalid token: " ++ t
  show UnexpectedEnd = "Unexpected end of input"
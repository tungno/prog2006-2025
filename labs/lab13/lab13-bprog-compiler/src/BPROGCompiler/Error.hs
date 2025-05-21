module BPROGCompiler.Error
  ( -- * Error types
    CompilerError(..)
  , ParserError(..)
  , TypeCheckError(..)
  , CodeGenError(..)
  ) where

import Data.Text (Text)

-- | Main compiler error type
data CompilerError
  = ParserErr ParserError        -- ^ Error during parsing
  | TypeCheckErr TypeCheckError  -- ^ Error during type checking
  | CodeGenErr CodeGenError      -- ^ Error during code generation
  | RuntimeErr Text              -- ^ Runtime error
  deriving (Eq)

-- | Show instance for CompilerError
instance Show CompilerError where
  show (ParserErr err) = "Parser error: " ++ show err
  show (TypeCheckErr err) = "Type error: " ++ show err
  show (CodeGenErr err) = "Code generation error: " ++ show err
  show (RuntimeErr msg) = "Runtime error: " ++ show msg

-- | Parser errors
data ParserError
  = UnexpectedToken Text         -- ^ Unexpected token
  | IncompleteString             -- ^ Unclosed string literal
  | IncompleteList               -- ^ Unclosed list
  | IncompleteQuotation          -- ^ Unclosed quotation
  | EmptyProgram                 -- ^ Empty program
  | InvalidNumber Text           -- ^ Invalid number format
  | UnbalancedBrackets Text      -- ^ Unbalanced brackets or braces
  deriving (Eq)

-- | Show instance for ParserError
instance Show ParserError where
  show (UnexpectedToken t) = "Unexpected token: " ++ show t
  show IncompleteString = "Unclosed string literal"
  show IncompleteList = "Unclosed list"
  show IncompleteQuotation = "Unclosed quotation (code block)"
  show EmptyProgram = "Empty program"
  show (InvalidNumber t) = "Invalid number format: " ++ show t
  show (UnbalancedBrackets t) = "Unbalanced brackets or braces: " ++ show t

-- | Type checking errors
data TypeCheckError
  = TypeMismatch Text Text       -- ^ Expected type vs. actual type
  | UndefinedSymbol Text         -- ^ Symbol not defined
  | NotAFunction Text            -- ^ Not a function
  | InvalidOperandType Text Text -- ^ Invalid operand type for operation
  | StackUnderflow               -- ^ Stack underflow
  | InvalidIfCondition           -- ^ Invalid if condition (not a boolean)
  deriving (Eq)

-- | Show instance for TypeCheckError
instance Show TypeCheckError where
  show (TypeMismatch expected actual) = 
    "Type mismatch: expected " ++ show expected ++ ", got " ++ show actual
  show (UndefinedSymbol name) = "Undefined symbol: " ++ show name
  show (NotAFunction name) = "Not a function: " ++ show name
  show (InvalidOperandType op typ) = 
    "Invalid operand type " ++ show typ ++ " for operation " ++ show op
  show StackUnderflow = "Stack underflow"
  show InvalidIfCondition = "If condition must be a boolean"

-- | Code generation errors
data CodeGenError
  = UnsupportedOperation Text    -- ^ Operation not supported in WAT
  | UnsupportedType Text         -- ^ Type not supported in WAT
  | MemoryAllocationError Text   -- ^ Error allocating memory
  | StringTooLong Int            -- ^ String exceeds maximum length
  | ListTooLarge Int             -- ^ List exceeds maximum size
  | TooManyLocals Int            -- ^ Too many local variables
  deriving (Eq)

-- | Show instance for CodeGenError
instance Show CodeGenError where
  show (UnsupportedOperation op) = "Unsupported operation in WebAssembly: " ++ show op
  show (UnsupportedType t) = "Unsupported type in WebAssembly: " ++ show t
  show (MemoryAllocationError msg) = "Memory allocation error: " ++ show msg
  show (StringTooLong len) = "String too long: " ++ show len ++ " characters"
  show (ListTooLarge size) = "List too large: " ++ show size ++ " elements"
  show (TooManyLocals count) = "Too many local variables: " ++ show count
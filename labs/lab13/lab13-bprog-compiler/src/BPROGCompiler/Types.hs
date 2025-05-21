module BPROGCompiler.Types 
  ( -- * Core types
    Token(..)
  , BPROGType(..)
  , BPROGExpr(..)
  , WasmType(..)
  , MemoryLayout(..)
  , SymbolInfo
  , SymbolTable
  , CompilerState(..)
  , emptyCompilerState
    -- * Compiler monad
  , Compiler
  , analyzeAST
  , throwError
  , catchError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import Control.Monad.IO.Class (MonadIO)
import BPROGCompiler.Error

-- | Token types for the parser
data Token
  = IntToken Integer           -- ^ Integer literal
  | FloatToken Double          -- ^ Float literal
  | BoolToken Bool             -- ^ Boolean literal
  | StringToken Text           -- ^ String literal
  | SymbolToken Text           -- ^ Symbol (function or variable name)
  | ListStartToken             -- ^ Opening bracket for list '['
  | ListEndToken               -- ^ Closing bracket for list ']'
  | QuotationStartToken        -- ^ Opening brace for quotation '{'
  | QuotationEndToken          -- ^ Closing brace for quotation '}'
  | OperatorToken Text         -- ^ Operator like '+', '-', '*', etc.
  deriving (Show, Eq)

-- | BPROG language type system
data BPROGType
  = IntType                    -- ^ Integer type
  | FloatType                  -- ^ Float type
  | BoolType                   -- ^ Boolean type
  | StringType                 -- ^ String type
  | ListType BPROGType         -- ^ List type with element type
  | QuotationType              -- ^ Quotation (code block) type
  | AnyType                    -- ^ Used for polymorphic operations
  | UnknownType                -- ^ Placeholder for type inference
  deriving (Show, Eq)

-- | WebAssembly value types
data WasmType
  = I32                        -- ^ 32-bit integer
  | I64                        -- ^ 64-bit integer
  | F32                        -- ^ 32-bit float
  | F64                        -- ^ 64-bit float
  | Pointer                    -- ^ Pointer to memory (represented as i32)
  | FuncRef                    -- ^ Function reference
  deriving (Show, Eq)

-- | AST expression types
data BPROGExpr
  = IntLit Integer             -- ^ Integer literal
  | FloatLit Double            -- ^ Float literal
  | BoolLit Bool               -- ^ Boolean literal
  | StringLit Text             -- ^ String literal
  | SymbolRef Text             -- ^ Symbol reference
  | ListLit [BPROGExpr]        -- ^ List literal
  | QuotationLit [BPROGExpr]   -- ^ Quotation literal (code block)
  | BinaryOp Text BPROGExpr BPROGExpr  -- ^ Binary operation
  | UnaryOp Text BPROGExpr     -- ^ Unary operation
  | StackOp Text               -- ^ Stack operation
  | IfThenElse BPROGExpr [BPROGExpr] [BPROGExpr]  -- ^ If-then-else statement
  | Loop BPROGExpr [BPROGExpr] -- ^ Loop with condition and body
  | Times BPROGExpr [BPROGExpr] -- ^ Times (repeat n times)
  | VarAssign Text BPROGExpr   -- ^ Variable assignment
  | FuncDef Text [BPROGExpr]   -- ^ Function definition
  | FuncCall Text [BPROGExpr]  -- ^ Function call
  | ListOp Text [BPROGExpr]    -- ^ List operation
  deriving (Show, Eq)

-- | Memory layout for WebAssembly
data MemoryLayout = MemoryLayout
  { memoryBase :: Int          -- ^ Base address
  , stringSection :: Int       -- ^ Start of string section
  , listSection :: Int         -- ^ Start of list section
  , currentAddress :: Int      -- ^ Current free address
  , stringTable :: [(Text, Int)] -- ^ Map of strings to addresses
  } deriving (Show, Eq)

-- | Type and address info for symbols
type SymbolInfo = (BPROGType, Maybe Int)

-- | Symbol table mapping names to types and addresses
type SymbolTable = [(Text, SymbolInfo)]

-- | Compiler state
data CompilerState = CompilerState
  { ast :: [BPROGExpr]         -- ^ Current AST
  , symbolTable :: SymbolTable -- ^ Symbol table with types
  , memoryLayout :: MemoryLayout -- ^ Memory layout information
  , watOutput :: Text          -- ^ Generated WebAssembly text
  , optimizationLevel :: Int   -- ^ Optimization level (0-3)
  , memoryExport :: Bool       -- ^ Whether to export memory
  , includePrelude :: Bool     -- ^ Whether to include prelude functions
  , defaultIntType :: WasmType -- ^ Default integer type (I32 or I64)
  , defaultFloatType :: WasmType -- ^ Default float type (F32 or F64)
  } deriving (Show)

-- | Empty initial compiler state
emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState
  { ast = []
  , symbolTable = []
  , memoryLayout = MemoryLayout 0 0 1024 0 []
  , watOutput = T.empty
  , optimizationLevel = 1
  , memoryExport = True
  , includePrelude = True
  , defaultIntType = I32
  , defaultFloatType = F32
  }

-- | The Compiler monad
type Compiler = ExceptT CompilerError IO

-- | Throw a compiler error
throwError :: CompilerError -> Compiler a
throwError = throwE

-- | Catch a compiler error
catchError :: Compiler a -> (CompilerError -> Compiler a) -> Compiler a
catchError = catchE

-- | Analyze the AST for types and generate symbol table
analyzeAST :: CompilerState -> Compiler CompilerState
analyzeAST state = do
  -- This is a placeholder for the actual implementation
  -- In a real compiler, this would perform type checking and analysis
  return state
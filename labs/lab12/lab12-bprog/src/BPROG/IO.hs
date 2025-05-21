module BPROG.IO
  ( -- * I/O operations
    print
  , read
    -- * String parsing operations
  , parseInteger
  , parseFloat
  , parseWords
  , dumpStack  -- Added for debugging
  ) where

import Prelude hiding (print, read)
import BPROG.Types
import BPROG.Error

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

-- | Print a value to standard output (x --)
print :: InterpreterState -> Interpreter InterpreterState
print state = case stack state of
  (value:rest) -> do
    liftIO $ putStrLn $ formatValue value
    liftIO $ hFlush stdout
    return state { stack = rest }
  _ -> throwError StackEmpty

-- | Read a line from standard input (-- s)
read :: InterpreterState -> Interpreter InterpreterState
read state = do
  input <- liftIO TIO.getLine
  return state { stack = StringVal input : stack state }

-- | Parse a string into an integer (s -- i)
parseInteger :: InterpreterState -> Interpreter InterpreterState
parseInteger state = case stack state of
  (StringVal s:rest) -> case readMaybe (T.unpack s) of
    Just n -> return state { stack = IntVal n : rest }
    Nothing -> throwError NumberConversionError
  _ -> throwError ExpectedString

-- | Parse a string into a float (s -- f)
parseFloat :: InterpreterState -> Interpreter InterpreterState
parseFloat state = case stack state of
  (StringVal s:rest) -> case readMaybe (T.unpack s) of
    Just n -> return state { stack = FloatVal n : rest }
    Nothing -> throwError NumberConversionError
  _ -> throwError ExpectedString

-- | Parse a string into a list of words (s -- list)
parseWords :: InterpreterState -> Interpreter InterpreterState
parseWords state = case stack state of
  (StringVal s:rest) -> do
    let wordsList = map StringVal (T.words s)
    return state { stack = ListVal wordsList : rest }
  _ -> throwError ExpectedString

-- | Format a value for printing
formatValue :: Value -> String
formatValue (IntVal n) = show n
formatValue (FloatVal n) = show n
formatValue (BoolVal True) = "True"
formatValue (BoolVal False) = "False"
formatValue (StringVal s) = "\"" ++ T.unpack s ++ "\""
formatValue (ListVal vs) = "[" ++ intercalate "," (map formatValue vs) ++ "]"
formatValue (QuotationVal tokens) = "{ " ++ unwords (map T.unpack tokens) ++ " }"
formatValue (SymbolVal s) = T.unpack s

-- | Dump the entire stack for debugging
dumpStack :: InterpreterState -> Interpreter InterpreterState
dumpStack state = do
  liftIO $ putStrLn "=== Stack Dump ==="
  liftIO $ mapM_ (\(i, val) -> putStrLn $ show i ++ ": " ++ formatValue val) 
                 (zip [0..] (stack state))
  liftIO $ putStrLn "================="
  return state
module Main (main) where

import BPROG
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Monad (when)

-- | Main entry point
main :: IO ()
main = do
  -- Set stdout to LineBuffering mode for better REPL experience
  hSetBuffering stdout LineBuffering
  
  -- Parse command-line arguments
  args <- getArgs
  
  case args of
    -- If no arguments are provided, start in REPL mode
    [] -> do
      putStrLn "Starting BPROG interpreter in REPL mode..."
      runREPL
      
    -- If a file path is provided, run in NORMAL mode
    [filePath] -> do
      putStrLn $ "Running BPROG program from file: " ++ filePath
      runProgramFile filePath
      
    -- If the first argument is -e or --eval, evaluate the provided code
    ("-e":code:_) -> do
      putStrLn "Evaluating BPROG code..."
      runProgram (T.pack code)
      
    ("--eval":code:_) -> do
      putStrLn "Evaluating BPROG code..."
      runProgram (T.pack code)
      
    -- Otherwise, print usage information
    _ -> do
      printUsage
  
-- | Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Usage: bprog [OPTION] [FILE]"
  putStrLn "Interpret BPROG code."
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -e, --eval CODE    Evaluate BPROG code"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  bprog                   # Start REPL mode"
  putStrLn "  bprog program.bprog     # Run a BPROG program from a file"
  putStrLn "  bprog -e \"3 5 +\"      # Evaluate BPROG code"
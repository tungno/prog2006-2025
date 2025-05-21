module BPROGCompiler
  ( -- * Main entry points
    runCompilerREPL
  , runCompilerFile
  , compileFile
  , compileFileWithOutput
  , compileText
    -- * Types
  , module BPROGCompiler.Types
    -- * Parser
  , module BPROGCompiler.Parser
    -- * Compiler
  , module BPROGCompiler.Compiler
    -- * Error handling
  , module BPROGCompiler.Error
  ) where

import BPROGCompiler.Types
import BPROGCompiler.Parser (parseProgram, parseLine, parseToken)
import BPROGCompiler.Compiler
import BPROGCompiler.Error
import BPROGCompiler.Prelude (loadPrelude)
import BPROGCompiler.CodeGen (generateWatModule)

import System.IO (hFlush, stdout)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline

-- | Run BPROG Compiler REPL
runCompilerREPL :: IO ()
runCompilerREPL = do
  putStrLn "BPROG to WebAssembly Compiler - REPL Mode"
  putStrLn "Type 'quit' or press Ctrl+D to exit."
  putStrLn "Type 'help' for available commands."
  
  -- Start the REPL loop
  runInputT defaultSettings $ loop emptyCompilerState
  where
    loop :: CompilerState -> InputT IO ()
    loop state = do
      minput <- getInputLine "bprog> "
      case minput of
        Nothing -> outputStrLn "Goodbye!"
        Just "quit" -> outputStrLn "Goodbye!"
        Just "help" -> do
          outputStrLn "Available commands:"
          outputStrLn "  help       - Show this help message"
          outputStrLn "  quit       - Exit the REPL"
          outputStrLn "  :wat       - Show the current WebAssembly text output"
          outputStrLn "  :ast       - Show the current AST"
          outputStrLn "  :symbols   - Show defined symbols and their types"
          outputStrLn "  :reset     - Reset the compiler state"
          outputStrLn "  <bprog code> - Compile BPROG code to WebAssembly"
          loop state
        Just ":wat" -> do
          liftIO $ TIO.putStrLn $ watOutput state
          loop state
        Just ":ast" -> do
          liftIO $ print $ ast state
          loop state
        Just ":symbols" -> do
          liftIO $ putStrLn "Symbol Table:"
          liftIO $ mapM_ (\(k, (t, _)) -> putStrLn $ "  " ++ T.unpack k ++ " : " ++ show t) (symbolTable state)
          loop state
        Just ":reset" -> do
          outputStrLn "Compiler state reset."
          loop emptyCompilerState
        Just input -> do
          result <- liftIO $ runExceptT $ compileLine (T.pack input) state
          case result of
            Left err -> do
              outputStrLn $ "Error: " ++ show err
              loop state
            Right newState -> do
              loop newState

-- | Run a BPROG compiler on a file
runCompilerFile :: FilePath -> IO ()
runCompilerFile filePath = do
  program <- TIO.readFile filePath
  result <- runExceptT $ compileText program
  case result of
    Left err -> putStrLn $ "Compilation error: " ++ show err
    Right watCode -> do
      putStrLn "Successfully compiled to WebAssembly text format:"
      TIO.putStrLn watCode

-- | Compile a BPROG program file to WAT
compileFile :: FilePath -> IO ()
compileFile filePath = do
  program <- TIO.readFile filePath
  result <- runExceptT $ compileText program
  case result of
    Left err -> putStrLn $ "Compilation error: " ++ show err
    Right watCode -> TIO.putStrLn watCode

-- | Compile a BPROG program file to WAT file with specified output path
compileFileWithOutput :: FilePath -> FilePath -> IO ()
compileFileWithOutput filePath outputPath = do
  program <- TIO.readFile filePath
  result <- runExceptT $ compileText program
  case result of
    Left err -> putStrLn $ "Compilation error: " ++ show err
    Right watCode -> do
      TIO.writeFile outputPath watCode
      putStrLn $ "Successfully compiled to WebAssembly text format: " ++ outputPath

-- | Compile a BPROG text program to WAT
compileText :: Text -> Compiler Text
compileText program = do
  -- Parse program into AST
  ast <- parseProgram program
  
  -- Set up initial compiler state
  let state = emptyCompilerState { ast = ast }
  
  -- Type check and analyze AST
  analysisState <- analyzeAST state
  
  -- Generate WebAssembly text format
  let watCode = generateWatModule analysisState
  
  return watCode

-- | Compile a single line of BPROG code
compileLine :: Text -> CompilerState -> Compiler CompilerState
compileLine line state = do
  -- Parse line into tokens
  tokens <- parseLine line
  
  -- Update the AST with new tokens
  let newAst = ast state ++ tokens
  let newState = state { ast = newAst }
  
  -- Type check and analyze the updated AST
  analysisState <- analyzeAST newState
  
  -- Generate WebAssembly text format
  let watCode = generateWatModule analysisState
  
  -- Return the updated state with new WAT output
  return $ analysisState { watOutput = watCode }
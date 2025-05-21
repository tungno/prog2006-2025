module Main (main) where

import qualified Data.Text.IO as TIO
import qualified BPROGCompiler
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Text (pack)
import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> BPROGCompiler.runCompilerREPL
    ["-h"] -> printUsage
    ["--help"] -> printUsage
    [filepath] -> BPROGCompiler.runCompilerFile filepath
    ["-c", filepath] -> BPROGCompiler.compileFile filepath
    ["-o", outputPath, filepath] -> BPROGCompiler.compileFileWithOutput filepath outputPath
    ["-e", code] -> compileExpr code
    _ -> do
      hPutStrLn stderr "Invalid arguments"
      printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "BPROG Compiler - A WebAssembly compiler for BPROG language"
  putStrLn "Usage:"
  putStrLn "  bprog-compiler                # Run the compiler REPL"
  putStrLn "  bprog-compiler FILE           # Compile and run a BPROG program"
  putStrLn "  bprog-compiler -c FILE        # Compile BPROG program to WAT, output to stdout"
  putStrLn "  bprog-compiler -o OUTPUT FILE # Compile BPROG program to WAT file"
  putStrLn "  bprog-compiler -e \"CODE\"      # Compile and run BPROG code from command line"
  putStrLn "  bprog-compiler -h             # Show this help message"

compileExpr :: String -> IO ()
compileExpr code = do
  result <- runExceptT $ BPROGCompiler.compileText (pack code)
  case result of
    Left err -> hPutStrLn stderr $ "Compilation error: " ++ show err
    Right watCode -> do
      putStrLn "Compiled WAT code:"
      TIO.putStrLn watCode
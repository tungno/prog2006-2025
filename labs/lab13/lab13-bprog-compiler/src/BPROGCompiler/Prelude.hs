module BPROGCompiler.Prelude
  ( -- * Prelude functions
    loadPrelude
  ) where

import BPROGCompiler.Types
import BPROGCompiler.Error
import BPROGCompiler.Parser (parseProgram)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Load the prelude functions
loadPrelude :: Compiler [BPROGExpr]
loadPrelude = do
  -- Try to load the prelude file
  let preludePath = "prelude" </> "prelude.bprog"
  fileExists <- liftIO $ doesFileExist preludePath
  
  if fileExists
    then do
      -- Load and parse the prelude file
      preludeContent <- liftIO $ TIO.readFile preludePath
      parseProgram preludeContent
    else do
      -- Use default prelude if file doesn't exist
      parseProgram defaultPrelude

-- | Default prelude content as a fallback
defaultPrelude :: Text
defaultPrelude = T.unlines
  [ T.pack "# BPROG Standard Prelude"
  , T.pack ""
  , T.pack "# Stack operations"
  , T.pack "over { 1 pick } fun"
  , T.pack "rot { swap >r swap r> } fun"
  , T.pack "nip { swap pop } fun"
  , T.pack "tuck { dup -rot } fun"
  , T.pack ""
  , T.pack "# Arithmetic functions"
  , T.pack "inc { 1 + } fun"
  , T.pack "dec { 1 - } fun"
  , T.pack "square { dup * } fun"
  , T.pack "cube { dup dup * * } fun"
  , T.pack ""
  , T.pack "# Boolean functions"
  , T.pack "and { && } fun"
  , T.pack "or { || } fun"
  , T.pack "not { not } fun"
  , T.pack "xor { 2dup and not -rot or and } fun"
  , T.pack ""
  , T.pack "# Comparison functions"
  , T.pack "!= { == not } fun"
  , T.pack "<= { 2dup < -rot == or } fun"
  , T.pack ">= { 2dup > -rot == or } fun"
  , T.pack ""
  , T.pack "# List functions"
  , T.pack "range { "
  , T.pack "  [] swap"  -- Initialize empty list
  , T.pack "  swap loop"
  , T.pack "  { dup 2dup >= }"  -- Condition: current >= end
  , T.pack "  {"
  , T.pack "    dup -rot cons swap 1 +"  -- Add current to list, increment
  , T.pack "  }"
  , T.pack "  swap pop"  -- Clean up loop counter, return list
  , T.pack "} fun"
  , T.pack ""
  , T.pack "# Math functions"
  , T.pack "abs { dup 0 < if { -1 * } { } } fun"
  , T.pack "max { 2dup < if { nip } { drop } } fun"
  , T.pack "min { 2dup > if { nip } { drop } } fun"
  , T.pack ""
  , T.pack "# Conditional execution"
  , T.pack "when { swap if { exec } { pop } } fun"
  , T.pack "unless { swap not if { exec } { pop } } fun"
  ]
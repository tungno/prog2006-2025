module BPROG.Prelude
  ( -- * Prelude functions
    loadPrelude
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

-- | Load the prelude from file
loadPrelude :: IO Text
loadPrelude = do
  let preludePaths = ["prelude/prelude.bprog", "prelude.bprog", "../prelude/prelude.bprog"]
  result <- foldr tryPath (return Nothing) preludePaths
  case result of
    Just content -> return content
    Nothing -> return T.empty
  where
    tryPath :: FilePath -> IO (Maybe Text) -> IO (Maybe Text)
    tryPath path fallback = do
      exists <- doesFileExist path
      if exists
        then do
          result <- try (TIO.readFile path) :: IO (Either SomeException Text)
          case result of
            Right content -> return (Just content)
            Left _ -> fallback
        else fallback
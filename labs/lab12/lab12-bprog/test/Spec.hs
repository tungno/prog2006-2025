module Main (main) where

import Test.Hspec
import OfficialTests (officialTests)
import BPROG
import qualified BPROG.ParserSpec
import qualified BPROG.InterpreterSpec
import qualified BPROG.OperationsSpec
import qualified BPROG.ExamplesSpec

-- Main test runner
main :: IO ()
main = hspec $ do
  describe "BPROG Interpreter" $ do
    -- Run the official tests from the lab specification
    officialTests
  
  -- Run the component tests
  describe "BPROG Components" $ do
    BPROG.ParserSpec.spec
    BPROG.InterpreterSpec.spec
    BPROG.OperationsSpec.spec
    BPROG.ExamplesSpec.spec
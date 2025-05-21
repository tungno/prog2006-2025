module BPROG.ExamplesSpec (spec) where

import Test.Hspec
import BPROG
import BPROG.Types
import BPROG.Error
import BPROG.Parser
import BPROG.Interpreter
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = describe "BPROG.Examples" $ do
  describe "Factorial example" $ do
    it "calculates factorial of 5" $ 
      pendingWith "Not implemented yet"

  describe "FizzBuzz example" $ do
    it "runs the FizzBuzz program" $ 
      pendingWith "Not implemented yet"
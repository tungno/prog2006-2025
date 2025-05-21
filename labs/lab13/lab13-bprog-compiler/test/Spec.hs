import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import BPROGCompiler.Types
import BPROGCompiler.Parser
import BPROGCompiler.Compiler
import BPROGCompiler.CodeGen
import BPROGCompiler.Error

import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = hspec $ do
  describe "BPROG Compiler" $ do
    describe "Parser" $ do
      it "should parse integer literals" $ do
        result <- runExceptT $ parseToken (T.pack "123")
        result `shouldBe` Right (IntToken 123)
      
      it "should parse float literals" $ do
        result <- runExceptT $ parseToken (T.pack "123.45")
        result `shouldBe` Right (FloatToken 123.45)
      
      it "should parse boolean literals" $ do
        result <- runExceptT $ parseToken (T.pack "True")
        result `shouldBe` Right (BoolToken True)
        
        result2 <- runExceptT $ parseToken (T.pack "False")
        result2 `shouldBe` Right (BoolToken False)
      
      it "should parse string literals" $ do
        result <- runExceptT $ parseToken (T.pack "\"hello\"")
        result `shouldBe` Right (StringToken (T.pack "hello"))
      
      it "should parse operators" $ do
        result <- runExceptT $ parseToken (T.pack "+")
        result `shouldBe` Right (OperatorToken (T.pack "+"))
        
        result2 <- runExceptT $ parseToken (T.pack "==")
        result2 `shouldBe` Right (OperatorToken (T.pack "=="))
    
    describe "Type Inference" $ do
      it "should infer types of numeric expressions" $ do
        -- Create test expressions
        let expr1 = IntLit 42
        let expr2 = FloatLit 3.14
        let expr3 = BinaryOp (T.pack "+") expr1 expr2
        
        -- Run type inference
        result1 <- runExceptT $ inferType expr1 emptyCompilerState
        result2 <- runExceptT $ inferType expr2 emptyCompilerState
        result3 <- runExceptT $ inferType expr3 emptyCompilerState
        
        -- Check results
        result1 `shouldBe` Right IntType
        result2 `shouldBe` Right FloatType
        result3 `shouldBe` Right FloatType  -- Result of int + float should be float
      
      it "should infer types of boolean expressions" $ do
        let expr1 = BoolLit True
        let expr2 = BinaryOp (T.pack "&&") expr1 expr1
        
        result1 <- runExceptT $ inferType expr1 emptyCompilerState
        result2 <- runExceptT $ inferType expr2 emptyCompilerState
        
        result1 `shouldBe` Right BoolType
        result2 `shouldBe` Right BoolType
    
    describe "Code Generation" $ do
      it "should generate WebAssembly for integer literals" $ do
        let state = emptyCompilerState { ast = [IntLit 42] }
        let wat = generateWatModule state
        
        T.isInfixOf (T.pack "i32.const 42") wat `shouldBe` True
      
      it "should generate WebAssembly for arithmetic operations" $ do
        let state = emptyCompilerState { 
                      ast = [
                        IntLit 2,
                        IntLit 3,
                        BinaryOp (T.pack "+") (IntLit 2) (IntLit 3)
                      ] 
                    }
        let wat = generateWatModule state
        
        T.isInfixOf (T.pack "i32.const 2") wat `shouldBe` True
        T.isInfixOf (T.pack "i32.const 3") wat `shouldBe` True
        T.isInfixOf (T.pack "i32.add") wat `shouldBe` True
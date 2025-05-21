module OfficialTests (officialTests) where

import Test.Hspec
import BPROG
import BPROG.Types
import BPROG.Error
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.Except (runExceptT)

-- | Helper function to test BPROG program output
testProgram :: String -> String -> Spec
testProgram input expected = it input $ do
  result <- runExceptT $ interpretProgram (T.pack input) emptyState
  case result of
    Left err -> expectationFailure $ "Error: " ++ show err
    Right state -> case stack state of
      [value] -> show value `shouldBe` expected
      _ -> expectationFailure $ "Expected one value on stack, got: " ++ show (stack state)

-- | Official tests from the lab specification
officialTests :: Spec
officialTests = describe "official tests for non-error programs" $ do
  -- literals
  testProgram "3"                           "3"
  testProgram "121231324135634563456363567" "121231324135634563456363567"
  testProgram "1.0"                         "1.0"
  testProgram "0.0"                         "0.0"
  testProgram "-1"                          "-1"
  testProgram "-1.1"                        "-1.1"
  testProgram "False"                       "False"
  testProgram "True"                        "True"
  testProgram "[ [ ] [ ] ]"                 "[[],[]]"
  testProgram "[ False [ ] True [ 1 2 ] ]"  "[False,[],True,[1,2]]"
  testProgram "\" [ so { not if ] and } \"" "\"[ so { not if ] and }\""
  
  -- quotation literals
  testProgram "{ 20 10 + }"             "{ 20 10 + }"
  testProgram "[ { + } { 10 + } { 20 10 + } ]"   "[{ + },{ 10 + },{ 20 10 + }]"
  
  -- simple arithmetic
  testProgram "1 1 +"               "2"       
  testProgram "10 20 *"             "200"
  testProgram "20 2 div"            "10"
  testProgram "20 2 /"              "10.0"
  
  -- arithmetic with type coercion
  testProgram "1 1.0 +"             "2.0"       
  testProgram "10 20.0 *"           "200.0"
  testProgram "20 2.0 div"          "10"
  testProgram "20.0 2.0 div"        "10"
  
  -- bool operations
  testProgram "False False &&"      "False"
  testProgram "False True ||"       "True"
  testProgram "False not"           "True"
  testProgram "True not"            "False"
  
  -- comparisons
  testProgram "20 10 <"             "False"
  testProgram "20 10 >"             "True"
  testProgram "20 10.0 >"           "True"
  testProgram "20.0 20.0 >"         "False"
  testProgram "10 10 =="            "True"
  testProgram "10 10.0 =="          "True"
  testProgram "True True =="        "True"
  testProgram "True 40 40 == =="    "True"
  testProgram "\" abba \" \" abba \" ==" "True"
  testProgram "[ ] [ ] =="          "True"
  testProgram "[ 1 2 ] [ 1 2 ] =="  "True"
  testProgram " [ [ ] ] [ [ ] ] ==" "True"
  
  -- stack operations
  testProgram "10 20 swap pop"          "20"
  testProgram "10 dup dup + swap pop"   "20"
  testProgram "10 20 swap dup + div"    "1"
  
  -- length
  testProgram "\" hello \" length"              "5"
  testProgram "\" hello world \" length"        "11"
  testProgram "[ 1 2 3 [ ] ] length"            "4"
  testProgram "{ 10 20 + } length"              "3"

  -- String parsing
  testProgram "\" 12 \" parseInteger"           "12"
  testProgram "\" 12.34 \" parseFloat"          "12.34"
  testProgram "\" adam bob charlie \" words"    "[\"adam\",\"bob\",\"charlie\"]"          
  
  -- lists
  testProgram "[ 1 2 3 ]"           "[1,2,3]"
  testProgram "[ 1 \" bob \" ]"     "[1,\"bob\"]"
  testProgram "[ 1 2 ] empty"       "False"
  testProgram "[ ] empty"           "True"
  testProgram "[ 1 2 3 ] head"      "1"
  testProgram "[ 1 2 3 ] length"    "3"
  testProgram "[ 1 2 3 ] tail"      "[2,3]"
  testProgram "1 [ ] cons"          "[1]"
  testProgram "1 [ 2 3 ] cons"      "[1,2,3]"
  testProgram "[ 1 ] [ 2 3 ] append" "[1,2,3]"
  testProgram "[ 1 2 ] [ ] append"  "[1,2]"
  testProgram "[ 1 ] [ 2 3 ] cons"  "[[1],2,3]"

  -- list quotations
  testProgram "[ 1 2 3 ] map { 10 * }"                              "[10,20,30]"
  testProgram "[ 1 2 3 ] map { 1 + }"                               "[2,3,4]"
  testProgram "[ 1 2 3 4 ] map { dup 2 > if { 10 * } { 2 * } }"     "[2,4,30,40]"
  testProgram "[ 1 2 3 4 ] each { 10 * } + + +"                     "100"
  testProgram "[ 1 2 3 4 ] 0 foldl { + }"                           "10"
  testProgram "[ 2 5 ] 20 foldl { div }"                            "2"
  
  -- note no { } needed for 1 instruction code
  testProgram "[ \" 1 \" \" 2 \" \" 3 \" ] each { parseInteger } [ ] cons cons cons" "[1,2,3]"
  testProgram "[ \" 1 \" \" 2 \" \" 3 \" ] each parseInteger [ ] 3 times cons"       "[1,2,3]"
  testProgram "[ 1 2 3 4 ] 0 foldl +"                               "10"
  testProgram "[ 2 5 ] 20 foldl div"                                "2"
  
  -- assignments
  testProgram "age"                             "age"
  testProgram "age 10 := age"                   "10"
  testProgram "10 age swap := age"              "10"
  testProgram "[ 1 2 3 ] list swap := list"     "[1,2,3]"
  testProgram "age 20 := [ 10 age ]"            "[10,20]"

  testProgram "inc { 1 + } fun 1 inc"           "2"
  testProgram "mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10" "110"
  
  -- quotations
  testProgram "{ 20 10 + } exec"                "30"
  testProgram "10 { 20 + } exec"                "30"
  testProgram "10 20 { + } exec"                "30"
  testProgram "{ { 10 20 + } exec } exec"       "30"
  testProgram "{ { 10 20 + } exec 20 + } exec"  "50"
  
  -- if
  testProgram "True if { 20 } { }"               "20"
  testProgram "True if { 20 10 + } { 3 }"        "30"
  testProgram "10 5 5 == if { 10 + } { 100 + }"  "20"
  testProgram "False if { } { 45 }"              "45"
  testProgram "True if { False if { 50 } { 100 } } { 30 }" "100"

  -- if without quotation, more ergonomic expressions
  testProgram "True if 20 { }"                 "20"
  testProgram "True if { 20 10 + } 3"          "30"
  testProgram "10 10 5 5 == if + { 100 + }"    "20"
  testProgram "False if { } 45"                "45"
  testProgram "True if { False if 50 100 } 30" "100"

  -- times
  testProgram "1 times { 100 50 + }"                               "150"
  testProgram "5 times { 1 } [ ] 5 times { cons } 0 foldl { + }"   "5"
  testProgram "5 times 1     [ ] 5 times   cons   0 foldl   +  "   "5"
  testProgram "5 times { 10 } + + + +"                             "50"
  testProgram "5 times 10 4 times +"                               "50"

  -- loop
  testProgram "1 loop { dup 4 > } { dup 1 + } [ ] 5 times { cons }"         "[1,2,3,4,5]"
  testProgram "1 loop { dup 4 > } { dup 1 + } [ ] 5 times   cons  "         "[1,2,3,4,5]"
  testProgram "[ 1 ] loop { dup length 9 > }  { dup head 1 + swap cons }"   "[10,9,8,7,6,5,4,3,2,1]"


  testProgram "odd { dup 2 div swap 2 / == if False True } fun \
     \ 2 odd"                                                        "False"
  
  testProgram "odd { dup 2 div swap 2 / == if False True } fun \
     \ 3 odd"                                                        "True"
  
  testProgram "toList { [ ] swap times cons } fun \
     \ 1 2 3 4 \
     \ 4 toList"                                                      "[1,2,3,4]"
  
  testProgram "gen1toNum { max swap := 1 loop { dup max > } { dup 1 + } } fun \
     \ 3 gen1toNum + + +"                                            "10"

  testProgram "odd { dup 2 div swap 2 / == if False True } fun \
     \ toList { [ ] swap times cons } fun \
     \ gen1toNum { max swap := 1 loop { dup max > } { dup 1 + } } fun \
     \ 4 gen1toNum 5 toList map odd"                            "[True,False,True,False,True]"
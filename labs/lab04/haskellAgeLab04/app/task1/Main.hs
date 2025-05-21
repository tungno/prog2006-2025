-- haskellAgeLab04/app/task1/Main.hs
module Main where

import Test.QuickCheck
import Task1 (mreverse, prop_mreverse_matches_reverse)

main :: IO ()
main = do
    putStrLn "Testing mreverse function..."

    -- Test with string
    let testStr = "Hello"
    putStrLn $ "Original string: " ++ testStr
    putStrLn $ "Reversed string: " ++ mreverse testStr

    -- Test with list of integers
    let testList = [1, 2, 3]
    putStrLn $ "Original list: " ++ show testList
    putStrLn $ "Reversed list: " ++ show (mreverse testList)

    -- Run QuickCheck test
    putStrLn "\nRunning QuickCheck test to verify mreverse against built-in reverse..."
    quickCheck prop_mreverse_matches_reverse

    putStrLn "All tests completed."
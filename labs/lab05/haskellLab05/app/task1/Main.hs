-- app/task1/Main.hs
module Main where

import Task1 (mhead1, mhead2, mhead3, mhead4, mhead5, mhead6)

main :: IO ()
main = do
    putStrLn "Testing mhead functions..."

    -- Test with string
    let testStr = "Hello"
    putStrLn $ "Original string: " ++ testStr
    putStrLn $ "mhead1: " ++ [mhead1 testStr]
    putStrLn $ "mhead2: " ++ [mhead2 testStr]
    putStrLn $ "mhead3: " ++ [mhead3 testStr]
    putStrLn $ "mhead4: " ++ [mhead4 testStr]
    putStrLn $ "mhead5: " ++ [mhead5 testStr]
    putStrLn $ "mhead6: " ++ [mhead6 testStr]

    -- Test with list of integers
    let testList = [1, 2, 3]
    putStrLn $ "Original list: " ++ show testList
    putStrLn $ "mhead1: " ++ show (mhead1 testList)
    putStrLn $ "mhead2: " ++ show (mhead2 testList)
    putStrLn $ "mhead3: " ++ show (mhead3 testList)
    putStrLn $ "mhead4: " ++ show (mhead4 testList)
    putStrLn $ "mhead5: " ++ show (mhead5 testList)
    putStrLn $ "mhead6: " ++ show (mhead6 testList)

    putStrLn "All tests completed."
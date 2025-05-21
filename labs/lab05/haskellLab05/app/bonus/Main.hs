-- app/bonus/Main.hs
module Main where

import Bonus (fibs, fibsZip, fib2, fib2Zip, count)

main :: IO ()
main = do
    putStrLn "Testing Bonus implementations..."

    -- Compare the first several elements of both fibs sequences
    putStrLn "First 15 elements of the original fibs sequence:"
    print $ take 15 fibs
    
    putStrLn "\nFirst 15 elements of the zipWith fibs sequence:"
    print $ take 15 fibsZip
    
    -- Verify they produce the same results
    putStrLn "\nVerifying both sequences give the same values:"
    let testRange = [0..10]
    mapM_ (\n -> putStrLn $ "fib2 " ++ show n ++ " = " ++ show (fib2 n) ++ 
                           ", fib2Zip " ++ show n ++ " = " ++ show (fib2Zip n)) testRange
    
    -- Test the count function
    putStrLn "\nTesting count function:"
    let testList = [2,10,3,10,4]
    putStrLn $ "count 10 " ++ show testList ++ " = " ++ show (count 10 testList)
    putStrLn $ "count 2 " ++ show testList ++ " = " ++ show (count 2 testList)
    putStrLn $ "count 5 " ++ show testList ++ " = " ++ show (count 5 testList)
    
    putStrLn "All tests completed."
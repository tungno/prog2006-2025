-- app/task2/Main.hs
module Main where

import Task2 (mfact)

main :: IO ()
main = do
    putStrLn "Testing mfact (factorial) function..."

    -- Test with various inputs
    putStrLn $ "mfact 0 = " ++ show (mfact 0)
    putStrLn $ "mfact 1 = " ++ show (mfact 1)
    putStrLn $ "mfact 5 = " ++ show (mfact 5)
    putStrLn $ "mfact 10 = " ++ show (mfact 10)
    
    -- For larger values, show that it works (though the numbers get very large)
    putStrLn $ "mfact 20 = " ++ show (mfact 20)
    
    putStrLn "All tests completed."
-- app/task3/Main.hs
module Main where

import Task3 (fib)

main :: IO ()
main = do
    putStrLn "Testing fib (Fibonacci) function..."

    -- Display the first few Fibonacci numbers
    putStrLn "First 15 Fibonacci numbers:"
    mapM_ (\n -> putStrLn $ "fib " ++ show n ++ " = " ++ show (fib n)) [0..14]
    
    putStrLn "All tests completed."
-- app/task4/Main.hs
module Main where

import Task4 (fibs, fib2)

main :: IO ()
main = do
    putStrLn "Testing fib2 function and fibs sequence..."

    -- Show the first several elements of the infinite fibs sequence
    putStrLn "First 15 elements of the fibs sequence:"
    print $ take 15 fibs

    -- Display the first few Fibonacci numbers using fib2
    putStrLn "\nFirst 15 Fibonacci numbers using fib2:"
    mapM_ (\n -> putStrLn $ "fib2 " ++ show n ++ " = " ++ show (fib2 n)) [0..14]
    
    putStrLn "All tests completed."
-- Declares module 'Main' that exports only 'main' function
module Main (main) where

-- Imports task4Main function from Task4 module
import Task4 (task4Main)

-- Type signature: main is an IO action
main :: IO ()
-- Implementation: runs task4Main
main = task4Main
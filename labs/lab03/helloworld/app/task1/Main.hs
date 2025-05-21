-- Declares module 'Main' that exports only 'main' function
module Main (main) where

-- Imports task1Main function from Task1 module
import Task1 (task1Main)

-- Type signature: main is an IO action
main :: IO ()

-- Implementation: runs task1Main
main = task1Main
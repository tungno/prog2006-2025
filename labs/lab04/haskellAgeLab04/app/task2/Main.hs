module Main where                               -- Defines the main module.

import Task2 (mulTable, pad)                    -- Imports the mulTable and pad functions from Task2 module.
import System.Environment (getArgs)             -- Imports the getArgs function to access command line arguments.
import Text.Read (readMaybe)                    -- Imports readMaybe for safely parsing strings into numbers.

main :: IO ()                                   -- Type signature for the main function that performs IO operations.
main = do                                       -- Begins the main IO action.
    putStrLn "Multiplication Table Generator"
    putStrLn "-----------------------------"

    -- Try to get table size from command line arguments
    args <- getArgs                             -- Gets command line arguments and binds them to args.
    tableSize <- case args of                   -- Begins a case expression to determine table size.
        [] -> do                                -- case handles when no arguments are provided, asking the user for input.
            putStrLn "Please enter the size of the multiplication table:"
            input <- getLine                    -- Gets user input for table size.
            case readMaybe input of             -- Tries to parse the user input as a number.
                Just n  -> return n             -- If parsing succeeds, use the provided number.
                Nothing -> do                   -- section - If parsing fails, use default size 5.
                    putStrLn "Invalid input. Using default size of 5."
                    return 5
        (arg:_) -> case readMaybe arg of       -- case handles when at least one argument is provided. -- Tries to parse the first argument as a number.
            Just n  -> return n                -- If parsing succeeds, use the provided number.
            Nothing -> do                       -- section - If parsing fails, use default size 5.
                putStrLn "Invalid argument. Using default size of 5."
                return 5

    -- Generate and display the multiplication table
    putStrLn $ "\nMultiplication table of size " ++ show tableSize ++ ":\n"
    putStr $ mulTable tableSize                 -- Generates and displays the multiplication table.

    -- Demonstrate the padding function
    putStrLn "\nPadding examples:"
    putStrLn $ "pad 3 5:   '" ++ pad 3 5 ++ "'"
    putStrLn $ "pad 3 42:  '" ++ pad 3 42 ++ "'"
    putStrLn $ "pad 3 123: '" ++ pad 3 123 ++ "'"
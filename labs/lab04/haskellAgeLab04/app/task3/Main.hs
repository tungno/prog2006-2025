module Main where

import Task3 (countOldestStudents, countOldestStudentsSinglePass, parseStudent, Student(..))
import System.Environment (getArgs)

main :: IO ()
main = do
    putStrLn "Oldest Students Counter"
    putStrLn "----------------------"

    -- Get input from stdin
    putStrLn "Reading student data from stdin..."
    input <- getContents

    -- Process data and find the oldest students
    let students = map parseStudent (lines input)
    let maxAge = maximum [age s | s <- students]
    let oldestStudents = filter (\s -> age s == maxAge) students

    -- Extract last names of oldest students
    let oldestLastNames = map lastName oldestStudents

    -- Output the result in the desired format
    putStrLn $ "The oldest people are " ++ formatLastNames oldestLastNames ++ ", thus the count of oldest students is " ++ show (length oldestStudents) ++ "."

-- Helper function to format a list of last names with commas and "and"
formatLastNames :: [String] -> String
formatLastNames [] = ""
formatLastNames [name] = name
formatLastNames [name1, name2] = name1 ++ " and " ++ name2
formatLastNames names =
    let allButLast = init names
        lastOne = last names
        commaList = concat (zipWith (\n _ -> n ++ ", ") (init allButLast) (init allButLast))
    in commaList ++ last allButLast ++ " and " ++ lastOne
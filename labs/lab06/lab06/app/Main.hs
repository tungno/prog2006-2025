-- lab06/app/Main.hs
module Main (main) where

import Lib
import Data.Either (isRight)

-- | Print detailed information about the message decoding process
printDetailedInfo :: String -> IO ()
printDetailedInfo msg = 
    let nums = parseInput msg
        minVal = findMin nums
        maxVal = findMax nums
        midValue = midpoint minVal maxVal
        count = countOccurrences midValue nums
        minUnique = isUnique minVal nums
        maxUnique = isUnique maxVal nums
        sumEven = isEven (minVal + maxVal)
    in do
        putStrLn $ "* Minimum number: " ++ show minVal ++ (if minUnique then " (unique!)" else " (not unique)")
        putStrLn $ "* Maximum number: " ++ show maxVal ++ (if maxUnique then " (unique!)" else " (not unique)")
        putStrLn $ "* Sum: " ++ show (minVal + maxVal) ++ (if sumEven then " (even and divisible by 2)" else " (odd)")
        putStrLn $ "* Min + Max divided by 2: " ++ show midValue ++ " (magic number!)"
        putStrLn $ "* Occurrences of " ++ show midValue ++ ": " ++ show count ++ " (our cosmic message!)"

main :: IO ()
main = do
    msg <- getContents
    let result = decodeMessageImproved msg
    
    -- Print detailed info if there's no error
    if isRight result
        then do
            printDetailedInfo msg
            case result of
                Right n -> putStrLn $ "* The message is " ++ show n ++ "!"
                Left _ -> return () -- This won't happen due to isRight check
        else
            case result of
                Left err -> putStrLn err
                Right _ -> return () -- This won't happen due to isRight check
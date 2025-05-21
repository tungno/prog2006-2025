module UsingBonusLib
    ( countScore
    ) where

-- | Generate the power-of-2 sequence: 1, 2, 4, 8, 16, ...
-- This function returns 2 raised to the power of n
--
-- >>> powerOfTwo 0
-- 1
--
-- >>> powerOfTwo 1
-- 2
--
-- >>> powerOfTwo 2
-- 4
--
-- >>> powerOfTwo 3
-- 8
powerOfTwo :: Int -> Int
powerOfTwo n = 2^n

-- | Count the total score of the five balls lottery game data.
-- The input data is a text string with many games. Each game is represented as a single line.
-- The first three numbers are the "Winning Numbers", and the next five are the lottery numbers.
-- Each row ends with end-of-line character.
-- Calculate the total score of the game by summing up the scores of each line.
--
-- >>> countScore "8 14 16 5 8 18 16 12\n8 14 16 5 8 14 16 14"
-- 12
countScore :: String -> Int
countScore = sum . map processLine . lines

-- | Process a single line of the input data.
-- Each line represents a game, the first three numbers are the "Winning Numbers", 
-- and the next five are the lottery numbers. Calculate the score for the single game.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
--
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
--
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
-- 
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
--
-- >>> processLine "35 35 35 1 5 6 35 16"
-- 32
--
-- >>> processLine "35 35 6 1 5 6 35 35"
-- 49
-- 
-- >>> processLine "35 35 35 1 5 6 35 35"
-- 96
-- 
processLine :: String -> Int
processLine line = calculateScore winningNumbers scoringNumbers
  where
    numbers = map read (words line) :: [Int]
    winningNumbers = take 3 numbers
    scoringNumbers = drop 3 numbers  -- Get all scoring numbers, not just 5

-- | Calculate the score for a single game.
-- The first parameter is the list of winning numbers.
-- The second parameter is the list of scoring numbers.
--
-- >>> calculateScore [8, 14, 16] [5, 8, 18, 16, 12]
-- 3
--
-- >>> calculateScore [8, 14, 16] [5, 8, 14, 16, 14]
-- 9
--
-- >>> calculateScore [35, 35, 35] [1, 5, 6, 35, 16]
-- 32
--
-- >>> calculateScore [35, 35, 6] [1, 5, 6, 35, 35]
-- 49
--
-- >>> calculateScore [35, 35, 35] [1, 5, 6, 35, 35]
-- 96
calculateScore :: [Int] -> [Int] -> Int
calculateScore winningNumbers scoringNumbers = 
    sum $ map (uncurry (scoreNumberWithCount winningNumbers)) $ countScoringNumbers scoringNumbers

-- | Count occurrences of each scoring number.
-- Returns a list of pairs (number, count) in the same order as they first appear.
--
-- >>> countScoringNumbers [5, 8, 14, 16, 14]
-- [(5,1),(8,1),(14,2),(16,1)]
countScoringNumbers :: [Int] -> [(Int, Int)]
countScoringNumbers nums = 
    -- Keep track of numbers in order of appearance
    let unique = getUniquePreservingOrder nums
    in [(n, countOccurrences n nums) | n <- unique]
  where
    -- Get unique elements while preserving their original order
    getUniquePreservingOrder :: [Int] -> [Int]
    getUniquePreservingOrder [] = []
    getUniquePreservingOrder (x:xs) = x : getUniquePreservingOrder (filter (/= x) xs)
    
    -- Count occurrences of an element in a list
    countOccurrences :: Int -> [Int] -> Int
    countOccurrences x = length . filter (== x)

-- | Score a single number based on the winning numbers and its occurrence count in scoring numbers.
-- The first parameter is the list of winning numbers.
-- The second parameter is the number to score.
-- The third parameter is how many times this number appears in the scoring numbers.
--
-- >>> scoreNumberWithCount [8, 14, 16] 8 1
-- 1
--
-- >>> scoreNumberWithCount [8, 14, 16] 14 1
-- 2
--
-- >>> scoreNumberWithCount [8, 14, 16] 14 2
-- 6
scoreNumberWithCount :: [Int] -> Int -> Int -> Int
scoreNumberWithCount winningNumbers number count
    | number `notElem` winningNumbers = 0
    | otherwise = sum [initialPoints * powerOfTwo (i-1) | i <- [1..count]]
  where
    initialPoints = baseScore number * winningMultiplier winningNumbers number

-- | Calculate the base score for a number using the power-of-2 sequence.
-- Single digits (1-9): 2^0 = 1 point
-- Numbers 10-19: 2^1 = 2 points
-- Numbers 20-29: 2^2 = 4 points
-- Numbers 30-39: 2^3 = 8 points
--
-- >>> baseScore 5
-- 1
--
-- >>> baseScore 12
-- 2
--
-- >>> baseScore 25
-- 4
--
-- >>> baseScore 35
-- 8
baseScore :: Int -> Int
baseScore n
    | n >= 1 && n <= 9   = powerOfTwo 0  -- 2^0 = 1
    | n >= 10 && n <= 19 = powerOfTwo 1  -- 2^1 = 2
    | n >= 20 && n <= 29 = powerOfTwo 2  -- 2^2 = 4
    | n >= 30 && n <= 39 = powerOfTwo 3  -- 2^3 = 8
    | otherwise          = 0

-- | Calculate the multiplier based on how many times the number appears in winning numbers.
-- Using the power-of-2 sequence:
-- If it appears once: 2^0 = 1 (multiplier of 1)
-- If it appears twice: 2^1 = 2 (multiplier of 2)
-- If it appears three times: 2^2 = 4 (multiplier of 4)
--
-- >>> winningMultiplier [8, 14, 16] 8
-- 1
--
-- >>> winningMultiplier [8, 8, 16] 8
-- 2
--
-- >>> winningMultiplier [8, 8, 8] 8
-- 4
winningMultiplier :: [Int] -> Int -> Int
winningMultiplier winningNumbers number =
    case length (filter (== number) winningNumbers) of
        1 -> powerOfTwo 0  -- 2^0 = 1
        2 -> powerOfTwo 1  -- 2^1 = 2
        3 -> powerOfTwo 2  -- 2^2 = 4
        _ -> 0  -- This should never happen if number is in winningNumbers
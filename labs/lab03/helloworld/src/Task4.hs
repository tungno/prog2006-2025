module Task4 
    ( task4Main
    , mhead1
    , mhead2
    , mhead3
    , mhead4
    , mhead5
    ) where

-- | mhead1 using pattern matching
--
-- >>> mhead1 [1,2,3]
-- 1
mhead1 :: [a] -> a
mhead1 (x:_) = x
mhead1 [] = error "Empty list"

-- | mhead2 using list indexing
--
-- >>> mhead2 [1,2,3]
-- 1
mhead2 :: [a] -> a
mhead2 xs = xs !! 0

-- | mhead3 using foldr
--
-- >>> mhead3 [1,2,3]
-- 1
mhead3 :: [a] -> a
mhead3 = foldr (\x _ -> x) (error "Empty list")

-- | mhead4 using take and last
--
-- >>> mhead4 [1,2,3]
-- 1
mhead4 :: [a] -> a
mhead4 = last . take 1

-- | mhead5 using list comprehension
--
-- >>> mhead5 [1,2,3]
-- 1
mhead5 :: [a] -> a
mhead5 xs = [x | (x,i) <- zip xs [(0::Int)..], i == 0] !! 0

-- Interactive program to test different mhead implementations
task4Main :: IO ()
task4Main = do
    putStrLn "Enter a list of numbers (comma-separated, e.g., 1,2,3):"
    input <- getLine
    let numbers = read ("[" ++ input ++ "]") :: [Int]
    putStrLn $ "mhead1: " ++ show (mhead1 numbers)
    putStrLn $ "mhead2: " ++ show (mhead2 numbers)
    putStrLn $ "mhead3: " ++ show (mhead3 numbers)
    putStrLn $ "mhead4: " ++ show (mhead4 numbers)
    putStrLn $ "mhead5: " ++ show (mhead5 numbers)
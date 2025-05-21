-- src/Bonus.hs
module Bonus
    ( fibs
    , fibsZip
    , fib2
    , fib2Zip
    , count
    ) where

-- | The original recursive definition of fibs
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

-- | fibs definition rewritten using zipWith
--
-- zipWith (+) creates a new list by adding corresponding elements from two lists
-- By using (tail fibs) as the second argument, we're effectively shifting the list
-- by one position, so we're adding each Fibonacci number with its successor
--
-- >>> take 10 fibsZip
-- [0,1,1,2,3,5,8,13,21,34]
--
-- >>> take 15 fibsZip == take 15 fibs
-- True
fibsZip :: [Integer]
fibsZip = 0 : 1 : zipWith (+) fibsZip (tail fibsZip)

-- | Original fib2 function using fibs
--
-- >>> fib2 0
-- 0
--
-- >>> fib2 5
-- 5
--
-- >>> fib2 10
-- 55
fib2 :: Integer -> Integer
fib2 n
  | n < 0     = error "fib2: negative argument"
  | otherwise = fibs !! fromIntegral n

-- | fib2 function using fibsZip
--
-- >>> fib2Zip 0
-- 0
--
-- >>> fib2Zip 5
-- 5
--
-- >>> fib2Zip 10
-- 55
--
-- >>> fib2 7 == fib2Zip 7
-- True
fib2Zip :: Integer -> Integer
fib2Zip n
  | n < 0     = error "fib2Zip: negative argument"
  | otherwise = fibsZip !! fromIntegral n

-- | One-liner function to count occurrences of a value in a list
--
-- >>> count 10 [2,10,3,10,4]
-- 2
--
-- >>> count 5 [1,2,3,4,5]
-- 1
--
-- >>> count 42 [42,42,42]
-- 3
--
-- >>> count 7 []
-- 0
--
-- Explanation:
-- - repeat 1: creates an infinite list [1,1,1,...]
-- - zipWith (\x y -> if x == val then y else 0) xs (repeat 1):
--   * Pairs each element of xs with 1
--   * Returns 1 when the element equals val, 0 otherwise
-- - sum: adds all the 1s (occurrences) together
count :: Eq a => a -> [a] -> Int
count val xs = sum (zipWith (\x y -> if x == val then y else 0) xs (repeat 1))

-- | count x xs = sum (zipWith (\a b -> if a == x then 1 else 0) xs (repeat 0))
-- | the same as : count x xs = sum [if a == x then 1 else 0 | a <- xs]

-- | count 10 [2,10,3,10,4]
-- | count 5 [5,5,5,1,2,3]
-- | count 'a' "banana"
-- | count True [True, False, True]
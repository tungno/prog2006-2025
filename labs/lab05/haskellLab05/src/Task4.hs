-- src/Task4.hs
module Task4
    ( fibs
    , fib2
    ) where

-- | fibs is an infinite list containing all Fibonacci numbers
--
-- The sequence is defined recursively:
-- fibs = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...]
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    -- t@(b:_) means "bind t to (b:_)", letting us use both t and b
    -- This is a pattern matching where:
    -- - b is the first element of the tail
    -- - t is the entire tail (b:_)
    next (a : t@(b:_)) = (a+b) : next t

-- | fib2 returns the n-th Fibonacci number using the infinite fibs sequence
--
-- >>> fib2 0
-- 0
--
-- >>> fib2 1
-- 1
--
-- >>> fib2 2
-- 1
--
-- >>> fib2 3
-- 2
--
-- >>> fib2 5
-- 5
--
-- >>> fib2 10
-- 55
--
-- >>> fib2 (-1)
-- *** Exception: fib2: negative argument
-- ...
-- >>> fib2 0 == fibs !! 0
-- True
--
-- >>> fib2 5 == fibs !! 5
-- True
fib2 :: Integer -> Integer
fib2 n
  | n < 0     = error "fib2: negative argument"
  | otherwise = fibs !! fromIntegral n
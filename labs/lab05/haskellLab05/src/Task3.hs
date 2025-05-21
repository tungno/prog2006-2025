-- src/Task3.hs
module Task3
    ( fib
    ) where

-- | fib calculates the n-th Fibonacci number
--
-- The Fibonacci sequence is defined as: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
-- For n = 0, the result is 0
-- For n = 1, the result is 1
-- For n > 1, the result is fib(n-1) + fib(n-2)
-- For n < 0, it throws an error
--
-- >>> fib 0
-- 0
--
-- >>> fib 1
-- 1
--
-- >>> fib 2
-- 1
--
-- >>> fib 3
-- 2
--
-- >>> fib 5
-- 5
--
-- >>> fib 10
-- 55
--
-- >>> fib (-1)
-- *** Exception: fib: negative argument
-- ...
fib :: Integer -> Integer
fib n
  | n < 0     = error "fib: negative argument"
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)
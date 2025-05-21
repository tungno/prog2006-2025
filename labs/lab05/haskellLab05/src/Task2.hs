-- src/Task2.hs
module Task2
    ( mfact
    ) where

-- | mfact calculates the factorial of a given integer
--
-- Factorial of n is defined as: 1 * 2 * ... * (n-1) * n
-- For n = 0, the factorial is defined as 1
-- For n < 0, it throws an error
--
-- >>> mfact 0
-- 1
--
-- >>> mfact 1
-- 1
--
-- >>> mfact 5
-- 120
--
-- >>> mfact 10
-- 3628800
--
-- >>> mfact (-1)
-- *** Exception: mfact: negative argument
-- ...
mfact :: Integer -> Integer
mfact n
  | n < 0     = error "mfact: negative argument"
  | n == 0    = 1
  | otherwise = n * mfact (n - 1)
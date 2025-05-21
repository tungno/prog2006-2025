-- src/Task1.hs
module Task1
    ( mhead1
    , mhead2
    , mhead3
    , mhead4
    , mhead5
    , mhead6
    ) where

-- | mhead1: Implementation using function argument pattern matching
--
-- >>> mhead1 [1,2,3]
-- 1
--
-- >>> mhead1 "Hello"
-- 'H'
--
-- >>> mhead1 []
-- *** Exception: mhead1: empty list
-- ...
-- CallStack (from HasCallStack):
--   error, called at src/Task1.hs:23:13 in main:Task1
-- *** Exception: mhead1: empty list
-- ...
mhead1 :: [a] -> a
mhead1 (x:_) = x
mhead1 [] = error "mhead1: empty list"

-- | mhead2: Implementation using function guards
--
-- >>> mhead2 [1,2,3]
-- 1
--
-- >>> mhead2 "Hello"
-- 'H'
--
-- >>> mhead2 []
-- *** Exception: mhead2: empty list
-- ...
mhead2 :: [a] -> a
mhead2 xs
  | null xs   = error "mhead2: empty list"
  | otherwise = xs !! 0

-- | mhead3: Implementation using if-else expression
--
-- >>> mhead3 [1,2,3]
-- 1
--
-- >>> mhead3 "Hello"
-- 'H'
--
-- >>> mhead3 []
-- *** Exception: mhead3: empty list
-- ...
mhead3 :: [a] -> a
mhead3 xs = if null xs then error "mhead3: empty list" else xs !! 0

-- | mhead4: Implementation using let-in expression
--
-- >>> mhead4 [1,2,3]
-- 1
--
-- >>> mhead4 "Hello"
-- 'H'
--
-- >>> mhead4 []
-- *** Exception: mhead4: empty list
-- ...
mhead4 :: [a] -> a
mhead4 xs = let firstElement = if null xs then error "mhead4: empty list" else xs !! 0
            in firstElement

-- | mhead5: Implementation using where expression
--
-- >>> mhead5 [1,2,3]
-- 1
--
-- >>> mhead5 "Hello"
-- 'H'
--
-- >>> mhead5 []
-- *** Exception: mhead5: empty list
-- ...
mhead5 :: [a] -> a
mhead5 xs = firstElement
  where
    firstElement = if null xs then error "mhead5: empty list" else xs !! 0

-- | mhead6: Implementation using case-of expression
--
-- >>> mhead6 [1,2,3]
-- 1
--
-- >>> mhead6 "Hello"
-- 'H'
--
-- >>> mhead6 []
-- *** Exception: mhead6: empty list
-- ...
mhead6 :: [a] -> a
mhead6 xs = case xs of
  []    -> error "mhead6: empty list"
  (x:_) -> x
-- haskellAgeLab04/src/Task1.hs 

-- this defines a module named "Task1" that exports two items: the function mreverse and the property test prop_mreverse_matches_reserver.
module Task1
    ( mreverse
    , prop_mreverse_matches_reverse
    ) where

-- this imports the QuickCheck library, which is used for property-based testing in Haskell.
import Test.QuickCheck

-- | mreverse is my own implementation of list reversal
--
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse [1,2,3]
-- [3,2,1]
-- mreverse is a function that takes a list of type a and returns a list of the same type a
mreverse :: [a] -> [a]          -- Type signature declaring mreverse takes a list of any type and returns a list of the same type.
mreverse = mreverseHelper []    --  Defines mreverse by calling a helper function with an empty list as the initial accumulator.
  where                         -- Begins a section of local definitions.
    mreverseHelper acc [] = acc -- Base case: when the input list is empty, return the accumulator.
    mreverseHelper acc (x:xs) = mreverseHelper (x:acc) xs -- Recursive case: put the first element at the front of the accumulator and continue with the rest of the list.

-- Property test to verify our implementation matches the built-in reverse
prop_mreverse_matches_reverse :: [Int] -> Bool -- Type signature for a property testing function.
prop_mreverse_matches_reverse xs = mreverse xs == reverse xs -- Tests if our mreverse function produces the same result as the built-in reverse function.
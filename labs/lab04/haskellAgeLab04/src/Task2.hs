module Task2   -- Defines a module named Task2 that exports two functions.
    ( mulTable
    , pad
    ) where

-- | pad is a utility function that pads a number with spaces to occupy exactly n characters
-- It assumes numbers up to 3 digits only
--
-- >>> pad 3 5
-- "  5"
--
-- >>> pad 3 42
-- " 42"
--
-- >>> pad 3 123
-- "123"
pad :: Int -> Int -> String         -- Type signature declaring pad takes two integers and returns a string.
pad width num = replicate (width - length numStr) ' ' ++ numStr  -- Creates a padded string with spaces before the number to reach the specified width.
  where numStr = show num           --  Local definition that converts the number to a string.

-- | Creates a single row of the multiplication table
mulRow :: Int -> Int -> String      -- Type signature for a helper function that creates one row of the multiplication table.
mulRow size row = unwords [pad 3 (row * col) | col <- [1..size]]  -- Creates a row by padding and joining products of row × column.

-- | mulTable creates a multiplication table of the given size
--
-- >>> putStr $ mulTable 3
--   1   2   3
--   2   4   6
--   3   6   9
mulTable :: Int -> String  -- Type signature declaring mulTable takes an integer and returns a string.
mulTable size = unlines [mulRow size row | row <- [1..size]]  -- Creates the table by generating rows and joining them with newlines.
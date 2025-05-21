module Task3
    ( countOldestStudents
    , countOldestStudentsSinglePass
    , parseStudent
    , Student(..)
    ) where                             -- Defines a module that exports two functions and a data type.

-- | Student record type to store student information
data Student = Student
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    } deriving (Show, Eq)                   -- Defines a Student record type with three fields and derives Show and Eq instances.

-- | Parse a single line of input into a Student record
--
-- >>> parseStudent "Alice Cooper 25"
-- Student {firstName = "Alice", lastName = "Cooper", age = 25}
parseStudent :: String -> Student           -- Type signature for a function that parses a string into a Student.
parseStudent line =                         -- Begins the function definition.
    let ws = words line                     -- Splits the input line into a list of words.
        name = ws !! 0                      -- Extracts the first name (first word).
        surname = ws !! 1                   -- Extracts the last name (second word).
        studentAge = read (ws !! 2) :: Int  -- Converts the third word to an integer age.
    in Student name surname studentAge      -- Creates and returns a Student record.

-- | Find the count of students who are the oldest in a dataset
-- The function works in O(n) time complexity as it makes a single pass
-- through the data to find the maximum age, and then another pass to count
-- students of that age.
--
-- >>> countOldestStudents "Alice Cooper 25\nAlice Boa 23\nBob Marley 23\nAlice Chains 25\nCharlie Brown 21\nCharlie Chaplin 25\nEve Wonder 24\nSandra White 21"
-- 3
countOldestStudents :: String -> Int                -- Type signature for a function that counts oldest students.
countOldestStudents input = length oldestStudents   -- Returns the count of oldest students.
  where
    students = map parseStudent (lines input)       -- Parses all lines into Student records.
    maxAge = maximum [age s | s <- students]        -- Finds the maximum age in the dataset.
    oldestStudents = filter (\s -> age s == maxAge) students    -- Gets all students with the maximum age.

-- | Alternative O(n) solution using a single pass through the data
-- This solution is faster as it only requires a single traversal
-- of the dataset.
--
-- >>> countOldestStudentsSinglePass "Alice Cooper 25\nAlice Boa 23\nBob Marley 23\nAlice Chains 25\nCharlie Brown 21\nCharlie Chaplin 25\nEve Wonder 24\nSandra White 21"
-- 3
countOldestStudentsSinglePass :: String -> Int          -- Type signature for the single-pass version.
countOldestStudentsSinglePass input = snd $ foldl updateState (0, 0) students   -- Uses fold to count oldest students.
  where
    students = map parseStudent (lines input)           -- Parses all lines into Student records.

    updateState :: (Int, Int) -> Student -> (Int, Int)  -- Type signature for helper function.
    updateState (maxAge, count) student                 --  Function that updates state (max age and count) for each student.
      | age student > maxAge = (age student, 1)         -- If we find a new maximum age, reset the count to 1.
      | age student == maxAge = (maxAge, count + 1)     -- If age equals current max, increment the count.
      | otherwise = (maxAge, count)                     -- Otherwise, keep state unchanged.

{-
Time Complexity Analysis:

1. First Implementation (countOldestStudents):
   - Parsing the input: O(n)
   - Finding the maximum age: O(n)
   - Filtering students with the maximum age: O(n)
   Total complexity: O(n)

2. Alternative Implementation (countOldestStudentsSinglePass):
   - Parsing the input: O(n)
   - Single-pass fold to find both max age and count: O(n)
   Total complexity: O(n)

Both solutions are O(n) where n is the number of students, but the second
solution is more efficient as it only requires a single pass through the data
after parsing.
-}
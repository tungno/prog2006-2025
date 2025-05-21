# Haskell Age Lab 04

This project demonstrates intermediate Haskell concepts including list processing, recursion, folding, and algorithm complexity analysis. The project contains three distinct tasks that showcase different aspects of functional programming.

## Project Overview

The lab consists of three tasks:

1. **Task 1**: Custom list reversal implementation without using the built-in `reverse` function
2. **Task 2**: Multiplication table generator with custom string padding
3. **Task 3**: Finding and counting the oldest students in a dataset with O(n) complexity

## Prerequisites

- [Haskell](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Setup and Building

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd labs/lab04/haskellAgeLab04
   ```

2. Build the project:
   ```bash
   stack build
   ```

## Running the Tasks

### Task 1: List Reversal

This task implements a custom `mreverse` function that reverses a list without using the built-in `reverse`. It includes QuickCheck property testing to verify correctness.

```bash
stack exec task1-exe
```

**Expected Output:**
```
Testing mreverse function...
Original string: Hello
Reversed string: olleH
Original list: [1,2,3]
Reversed list: [3,2,1]

Running QuickCheck test to verify mreverse against built-in reverse...
+++ OK, passed 100 tests.
All tests completed.
```

### Task 2: Multiplication Table

This task generates and displays a formatted multiplication table of a specified size. The formatting ensures proper alignment using a custom padding function.

```bash
# Run with default table size of 5
stack exec task2-exe

# Or specify a custom size
stack exec task2-exe -- 7
```

**Example Output (for size 4):**
```
Multiplication Table Generator
-----------------------------

Multiplication table of size 4:

  1   2   3   4
  2   4   6   8
  3   6   9  12
  4   8  12  16

Padding examples:
pad 3 5:   '  5'
pad 3 42:  ' 42'
pad 3 123: '123'
```

### Task 3: Oldest Students Counter

This task processes a list of students (with name, surname, and age) and counts how many students are of the maximum age. It implements both a regular and an optimized single-pass approach.

```bash
# Run with input from stdin
echo "Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21" | stack exec task3-exe
```

**Example Output:**
```
Oldest Students Counter
----------------------
Reading student data from stdin...
The oldest people are Cooper, Chains and Chaplin, thus the count of oldest students is 3.
```

## Implementation Details

### Task 1: List Reversal

The `mreverse` function is implemented using an accumulator pattern for efficiency:

```haskell
mreverse :: [a] -> [a]
mreverse = mreverseHelper []
  where
    mreverseHelper acc [] = acc
    mreverseHelper acc (x:xs) = mreverseHelper (x:acc) xs
```

The implementation has O(n) time complexity where n is the length of the list.

### Task 2: Multiplication Table

The table generation uses list comprehensions to create rows and columns:

```haskell
mulTable :: Int -> String
mulTable size = unlines [mulRow size row | row <- [1..size]]

mulRow :: Int -> Int -> String
mulRow size row = unwords [pad 3 (row * col) | col <- [1..size]]
```

A custom padding function ensures proper alignment:

```haskell
pad :: Int -> Int -> String
pad width num = replicate (width - length numStr) ' ' ++ numStr
  where numStr = show num
```

### Task 3: Oldest Students Counter

Two implementations are provided with O(n) complexity:

1. **Two-pass approach**:
   ```haskell
   countOldestStudents :: String -> Int
   countOldestStudents input = length oldestStudents
     where
       students = map parseStudent (lines input)
       maxAge = maximum [age s | s <- students]
       oldestStudents = filter (\s -> age s == maxAge) students
   ```

2. **Single-pass approach** (more efficient):
   ```haskell
   countOldestStudentsSinglePass :: String -> Int
   countOldestStudentsSinglePass input = snd $ foldl updateState (0, 0) students
     where
       students = map parseStudent (lines input)
       
       updateState :: (Int, Int) -> Student -> (Int, Int)
       updateState (maxAge, count) student
         | age student > maxAge = (age student, 1)
         | age student == maxAge = (maxAge, count + 1)
         | otherwise = (maxAge, count)
   ```

The single-pass approach uses a fold to find both the maximum age and count in a single traversal, making it more efficient.

## Testing

Each task includes doctests and examples. Task 1 also includes QuickCheck property tests.

To run the tests:

```bash
stack test
```

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.
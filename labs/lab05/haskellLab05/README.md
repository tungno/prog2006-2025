# Haskell Lab 05

This project demonstrates advanced Haskell function implementation patterns, recursive definitions, and functional programming techniques. It focuses on exploring different ways to write the same function, understanding recursive patterns, and optimizing computations.

## Project Overview

The lab consists of four tasks and a bonus challenge:

1. **Task 1**: Multiple implementations of a list `head` function using different Haskell patterns
2. **Task 2**: Factorial function implementation using recursion
3. **Task 3**: Fibonacci sequence implementation with direct recursion
4. **Task 4**: Fibonacci sequence as an infinite list with recursive definition
5. **Bonus**: Optimization using `zipWith` and creating a one-liner `count` function

## Prerequisites

- [Haskell](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Setup and Building

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd haskellLab05
   ```

2. Build the project:
   ```bash
   stack build
   ```

3. Run tests to ensure everything works:
   ```bash
   stack test
   ```

## Running the Tasks

### Task 1: Multiple `head` Implementations

This task demonstrates six different ways to implement the same function (returning the first element of a list) using various Haskell patterns:

```bash
stack exec task1-exe
```

**Expected Output:**
```
Testing mhead functions...
Original string: Hello
mhead1: H
mhead2: H
mhead3: H
mhead4: H
mhead5: H
mhead6: H
Original list: [1,2,3]
mhead1: 1
mhead2: 1
mhead3: 1
mhead4: 1
mhead5: 1
mhead6: 1
All tests completed.
```

### Task 2: Factorial Implementation

This task implements a factorial function using recursion:

```bash
stack exec task2-exe
```

**Expected Output:**
```
Testing mfact (factorial) function...
mfact 0 = 1
mfact 1 = 1
mfact 5 = 120
mfact 10 = 3628800
mfact 20 = 2432902008176640000
All tests completed.
```

### Task 3: Recursive Fibonacci

This task implements the Fibonacci sequence using direct recursion:

```bash
stack exec task3-exe
```

**Expected Output:**
```
Testing fib (Fibonacci) function...
First 15 Fibonacci numbers:
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib 4 = 3
fib 5 = 5
...
fib 14 = 377
All tests completed.
```

### Task 4: Infinite Fibonacci Sequence

This task demonstrates how to define an infinite recursive Fibonacci sequence:

```bash
stack exec task4-exe
```

**Expected Output:**
```
Testing fib2 function and fibs sequence...
First 15 elements of the fibs sequence:
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]

First 15 Fibonacci numbers using fib2:
fib2 0 = 0
fib2 1 = 1
fib2 2 = 1
...
fib2 14 = 377
All tests completed.
```

### Bonus: Optimization with `zipWith` and One-Liner `count`

The bonus task shows how to rewrite the recursive Fibonacci definition using `zipWith` and implements a one-liner function to count occurrences in a list:

```bash
stack exec bonus-exe
```

**Expected Output:**
```
Testing Bonus implementations...
First 15 elements of the original fibs sequence:
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]

First 15 elements of the zipWith fibs sequence:
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]

Verifying both sequences give the same values:
fib2 0 = 0, fib2Zip 0 = 0
fib2 1 = 1, fib2Zip 1 = 1
...
fib2 10 = 55, fib2Zip 10 = 55

Testing count function:
count 10 [2,10,3,10,4] = 2
count 2 [2,10,3,10,4] = 1
count 5 [2,10,3,10,4] = 0
All tests completed.
```

## Implementation Details

### Task 1: Multiple `head` Implementations

The task implements the same function in six different ways:

1. **Pattern Matching**:
   ```haskell
   mhead1 :: [a] -> a
   mhead1 (x:_) = x
   mhead1 [] = error "mhead1: empty list"
   ```

2. **Guards**:
   ```haskell
   mhead2 :: [a] -> a
   mhead2 xs
     | null xs   = error "mhead2: empty list"
     | otherwise = xs !! 0
   ```

3. **If-Else Expression**:
   ```haskell
   mhead3 :: [a] -> a
   mhead3 xs = if null xs then error "mhead3: empty list" else xs !! 0
   ```

4. **Let-In Expression**:
   ```haskell
   mhead4 :: [a] -> a
   mhead4 xs = let firstElement = if null xs then error "mhead4: empty list" else xs !! 0
               in firstElement
   ```

5. **Where Expression**:
   ```haskell
   mhead5 :: [a] -> a
   mhead5 xs = firstElement
     where
       firstElement = if null xs then error "mhead5: empty list" else xs !! 0
   ```

6. **Case-Of Expression**:
   ```haskell
   mhead6 :: [a] -> a
   mhead6 xs = case xs of
     []    -> error "mhead6: empty list"
     (x:_) -> x
   ```

### Task 2: Factorial

The factorial implementation uses recursive pattern with guards:

```haskell
mfact :: Integer -> Integer
mfact n
  | n < 0     = error "mfact: negative argument"
  | n == 0    = 1
  | otherwise = n * mfact (n - 1)
```

### Task 3: Recursive Fibonacci

The recursive Fibonacci implementation:

```haskell
fib :: Integer -> Integer
fib n
  | n < 0     = error "fib: negative argument"
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)
```

### Task 4: Infinite Fibonacci Sequence

The infinite Fibonacci sequence is defined recursively:

```haskell
fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t
```

And a more efficient function to get the n-th Fibonacci number:

```haskell
fib2 :: Integer -> Integer
fib2 n
  | n < 0     = error "fib2: negative argument"
  | otherwise = fibs !! fromIntegral n
```

### Bonus: ZipWith Implementation

The Fibonacci sequence can be elegantly rewritten using `zipWith`:

```haskell
fibsZip :: [Integer]
fibsZip = 0 : 1 : zipWith (+) fibsZip (tail fibsZip)
```

And an elegant one-liner to count occurrences:

```haskell
count :: Eq a => a -> [a] -> Int
count val xs = sum (zipWith (\x y -> if x == val then y else 0) xs (repeat 1))
```

## Pattern Explanation

- **`t@(b:_)`** (in Task 4): A pattern binding where `t` refers to the entire tail (`(b:_)`) and `b` is the first element of that tail.
- **`zipWith (+) fibsZip (tail fibsZip)`** (in Bonus): Creates a new list by adding corresponding elements from `fibsZip` and its tail, effectively generating the next Fibonacci number in the sequence.

## Running Tests

The project includes doctests for each function. To run all tests:

```bash
stack test
```


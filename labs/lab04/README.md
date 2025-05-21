# Lab 04: Haskell age

* Deadline (soft): Friday, 28th of February
* Deadline (hard): Friday, 14th of March
* Score: 5 (+1)
* ChatGPT: **use only** for explanations. Do not use to generate code.
* Co-pilot: **do not use**
* Coding: code it all yourself, from memory
* Resources:
   * Haskell books
   * GHCi (interactive interpreter)
   * hoogle.haskell.org


# Objectives

* Basics of Haskell types:
   * lists
   * tuples
   * tuples used as maps to have key-value stores
* Basics of list processing in Haskell
   * with recursion
   * with folding



# Instructions

## Task 1: reverse a list

Write your own function, that given a list, returns the same list in reverse order.
There is a built-in function called `reverse` but you cannot use it for your own
implementation. However, you can use it for `property tests` with Quickcheck.

`mreverse` should work for any lists, and the following doctests should pass:

```haskell
-- | mreverse is my own implementation of list reversal
--
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse [1,2,3]
-- [3,2,1]
```


## Task 2: Multiplication table
Write a program that prints a matrix for Multiplication for a given size, nicely formatted on the screen, eg. for 5

```haskell
mulTable 5

 1  2  3  4  5
 2  4  6  8 10
 3  6  9 12 15
 4  8 12 16 20
 5 10 15 20 25
 ```

Note, you can make an assumption to only do table up to three-digit numbers,
and can pad everything to occupy 3 ascii slots.
The padding is part of the task, and you should write your own padding
function as an utility function.




## Task 3: Oldest students count

Given an input file in specific format, find which students are the oldest, and count them.
The data format follows simple rules:
* the first item in every line is the name of the student
* the second item, separated by whitespace is the student surname
* the third item, again separated by whitespace, is the student age, as integer value

Note: use `getContents`, `lines` and `words` to parse the input file.
The use of `parsec` library to parse input files is forbidden.

# Example

For the following file, the answer should be 3.
The oldest people are Cooper, Chains and Chaplin,
thus the count of oldest students is 3.

```rust
Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21
```


## Considerations for Task 3

* What is the expected complexity of your solution for Task 3? 
Use big-O notation and estimate time complexity in a function of N, the size of the initial data set.
* Can you improve it? Can you limit your traversals of the data to a single pass of the initial data?
What else we will need to traverse? How big is it?
* Remember, sorting a list takes approx. O(n * log n) therefore it already constitutes a performance penalty.
* Think about the simplicity and how the solution is expressed, and what consequences it might have for the performance.
* **NOTE** - if your solution is `O(n)`, you get extra `+1`.

# Lab 05: Haskell excercises

* Deadline (soft): Friday, 7th of March
* Deadline (hard): Friday, 14th of March
* Score: 4 (+2)
* Difficulty: medium
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
* Basic functions declarations
   * function pattern matching
   * guards
   * `where` notation
   * `let ... in ...` notation

* You should **not** use ChatGPT/Copilot. If you do, make it generate code
  as specified in the instructions.  Do not copy-paste code from
  ChatGPT/Copilot without READING it and understanding it.



# Instructions

## doctests

* make sure that ALL your implemented functions have at least few test cases as doctests
* make sure that `stack test` executes all your doctests and they are all OK


## hspec and QuickCheck

* you do not have to use hspec or QuickCheck unless otherwise specified


## Task 1: `mhead` - first element of a list

Haskell has many built-in functions to operate on lists.
As exercises we will re-define them as our own functions, and prefix them with `m`.
**NOTE** - we making exactly the same functions as standard functions, so if the
standard function throws an error, your function must also throw an error.
Your own function **must** have the same signature as the standard function.

`head` is a standard function that returns the first element of a given list.

Your task is to write several variants of your own function that returns the first element of the list, without using the built-in funtion `head`
Name your functions: mhead1, mhead2, and so on, following the requirements:

1. In this variant, use function argument pattern matching.
2. In this variant, use function guards
3. In this version, write the definition in a single line with `if ... else ...` expressions
4. In this variant, make use of `let .. in ..`
5. In this variant, use `where` expression
6. In this variant, use `case .. of ..` expression

**Note:** Destructuring is a form of pattern matching. Use it.



## Task 2: factorial

* Factorial of `n` means: `1 * 2 * ... * (n-1) * n`
* Write factorial function, called `mfact` that takes an integer, and 
  returns a factorial of this integer.
* You should write a `doctest` test cases in the documentation of your implementation.



# Task 3: fibonacci function

* Fibonacci sequence is a sequence that looks like that: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
* In general, if `fib n` returns the `n-th` fibonacci number, the sequence is: 0, 1, ...., (fib n) + fib (n - 1)
* Write a recursive `fib n` function that takes an integer and returns the `n-th` fibonacci number.



# Task 4: fibonacci sequence

Haskell can represent not only infinite sequences like `[1..]`, but also, it can represent
infinite sequences that are recursively defined. That means, the sequence can be defined
based on its own definition, recursively.  One such definition of a sequence that represents
infinite sequence of all fibonacci numbers is this:

```haskell
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t
```

Here, we are defining `fibs` as a sequence recursively, because fibs is used inside its own declaration.
* Study this sequence.
* Q: What is `(b:_)` for?
* Q: What does the `t@` do?
* Write a new version of your `fib n` function, call it `fib2 n` that uses fib sequence defined above,
  and returns the `n-th` element from that sequence.


# Bonus (+2): zip, zipWith

Rewrite the recursive declaration of `fibs` with the use of `zipWith` function.
Study it, and understand how `zip` and `zipWith` works.
To get +1 you will be asked a "secret" task, one-liner, to write, without the use of ChatGPT.

* An example bonus question for an extra +1 point (you have to write it for the TA by hand with no aid):
- Using `sum, repeat`, and `zipWith`, write a one-liner function,
  that counts how many times a given number appears in a given list.
  For example, how many times, number 10, appears in a given list.
  `count 10 [2,10,3,10,4]` should return 2
* Guide
- `sum` is a built-in function that sums adds together all elements of a list with numbers,
   e.g. `sum [1,2,3]` returns 6
- `repeat a` is a built-in function that generates an infinite sequence/list consisting of `a`, that is: [a,a,a,...].

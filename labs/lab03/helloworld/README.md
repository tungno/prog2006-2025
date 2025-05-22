# Haskell Basics Lab

This project demonstrates fundamental Haskell concepts including basic syntax, function declarations, pattern matching, and various approaches to list manipulation. Each task explores different aspects of Haskell programming.

## Project Overview

The project consists of four tasks:

1. **Task 1**: A simple "Hello World" program demonstrating basic IO
2. **Task 2**: Interactive name input and greeting
3. **Task 3**: Age calculator with type safety demonstrations
4. **Task 4**: Multiple implementations of a list head function

## Prerequisites

- [Haskell](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Setup and Building

1. Clone the repository:
   ```
   git clone https://github.com/tungno/prog2006-2025.git
   cd labs/lab03/helloworld
   ```

2. Build the project:
   ```
   stack build
   ```

## Running the Tasks

### Task 1: Hello World

A simple program that prints "Hello World" to standard output.

```bash
stack exec task1-exe
```

**Expected Output:**
```
Hello World
```

### Task 2: Hello Name

This program asks for your name and then greets you.

```bash
stack exec task2-exe
```

**Example Interaction:**
```
What is your name?
John
Hello John
```

### Task 3: Age Calculator

This program demonstrates type safety with a custom `Age` type. It asks for your name and age, then tells you your age in 10 years.

```bash
stack exec task3-exe
```

**Example Interaction:**
```
Hi, what is your name?
John
and what is your age?
25
Hello John, in 10 years you will be 35.
```

The implementation demonstrates two approaches to type safety:
- **Version A**: Uses private constructors and smart constructors to enforce data validation
- **Version B**: Uses public constructors for more flexible but less safe operations

### Task 4: Multiple List Head Implementations

This task demonstrates five different ways to implement a function that returns the first element of a list.

```bash
stack exec task4-exe
```

**Example Interaction:**
```
Enter a list of numbers (comma-separated, e.g., 1,2,3):
5,10,15,20
mhead1: 5
mhead2: 5
mhead3: 5
mhead4: 5
mhead5: 5
```

## Exploring the Code

Each task has two main components:
- A module in `src/` containing the implementation
- A main executable in `app/taskX/Main.hs` that runs the task

To explore the implementations:

```bash
# View Task 1 implementation
cat src/Task1.hs

# View Task 2 implementation
cat src/Task2.hs

# View Task 3 implementation
cat src/Task3.hs

# View Task 4 implementation
cat src/Task4.hs
```

## Interactive Exploration with GHCi

You can also explore the functions interactively using GHCi:

```bash
stack ghci
```

Once in GHCi, you can import and test individual functions:

```haskell
-- Import and test Task1
import Task1
task1Main

-- Import and test Task4's head implementations
import Task4
mhead1 [1,2,3]
mhead2 [1,2,3]
mhead3 [1,2,3]
mhead4 [1,2,3]
mhead5 [1,2,3]
```

## Task 3 Type Safety Explanation

Task 3 demonstrates two approaches to type safety:

1. **Version A (Exported in the module)**: 
   - Uses a private constructor `MkAge`
   - Provides a smart constructor `makeAge` that validates the input
   - Prevents creating invalid `Age` values (negative ages)
   - Better for public APIs and maintaining data invariants

2. **Version B (Commented out in the code)**:
   - Uses a public constructor `Age`
   - Allows direct creation of `Age` values without validation
   - More flexible but less safe
   - Suitable for internal, trusted code

## Task 4 Implementation Approaches

Task 4 shows five different ways to implement the `head` function:

1. **Pattern Matching**: `mhead1 (x:_) = x`
2. **List Indexing**: `mhead2 xs = xs !! 0`
3. **Using foldr**: `mhead3 = foldr (\x _ -> x) (error "Empty list")`
4. **Using take and last**: `mhead4 = last . take 1`
5. **List Comprehension**: `mhead5 xs = [x | (x,i) <- zip xs [(0::Int)..], i == 0] !! 0`

Each approach demonstrates different Haskell techniques and paradigms.

## Running Tests

The project includes doctest examples. You can run tests with:

```bash
stack test
```


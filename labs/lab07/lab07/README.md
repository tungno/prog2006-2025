# Lottery Points Calculator

This Haskell project implements a lottery points calculation system, demonstrating functional programming concepts such as function composition, list processing, and modular code organization without using do-notation.

## Project Overview

The lottery game has the following rules:

1. **Winning Numbers**: The first 3 numbers of each sequence are the "winning numbers"
2. **Scoring Numbers**: The next 5 numbers are the "scoring numbers"
3. **Points System**:
   - Numbers 1-9: 1 point
   - Numbers 10-19: 2 points
   - Numbers 20-29: 4 points
   - Numbers 30-39: 8 points
4. **Multipliers**:
   - Each additional occurrence of a winning number in the scoring numbers doubles its points (1st = base, 2nd = 2x, 3rd = 4x, etc.)
   - Duplicates in the winning numbers multiply the base points (2x for doubles, 4x for triples)

## Prerequisites

- [Haskell](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Setup and Building

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd lab07
   ```

2. Build the project:
   ```bash
   stack build
   ```

3. Run tests to ensure everything works:
   ```bash
   stack test
   ```

## Running the Program

The program takes input from stdin and outputs the total score:

```bash
cat data.txt | stack run
```

Or interactively:

```bash
stack run
8 14 16 5 8 18 16 12
8 14 16 5 8 14 16 14
[Ctrl+D to send EOF]
```

## Example Inputs and Expected Outputs

### Example 1: Basic Scoring

Input:
```
8 14 16 5 8 18 16 12
```

Output:
```
The total score is: 3
```

Explanation:
- `8` in scoring numbers matches winning number `8` = 1 point (single digit)
- `16` in scoring numbers matches winning number `16` = 2 points (10-19 range)
- Total: 3 points

### Example 2: With Duplicates

Input:
```
8 14 16 5 8 14 16 14
```

Output:
```
The total score is: 9
```

Explanation:
- `8` matches = 1 point (single digit)
- First `14` matches = 2 points (10-19 range)
- Second `14` matches = 4 points (doubled because of repetition)
- `16` matches = 2 points (10-19 range)
- Total: 9 points

### Example 3: With Winning Number Repetition

Input:
```
35 35 35 1 5 6 35 35
```

Output:
```
The total score is: 96
```

Explanation:
- Winning numbers `35` appears 3 times = 4x multiplier
- Base score for `35` = 8 points (30-39 range)
- First `35` in scoring = 8 × 4 = 32 points
- Second `35` in scoring = 32 × 2 = 64 points
- Total: 96 points

## Implementation Details

### Key Components

1. **Main Functions**:
   - `countScore`: Processes the entire input by splitting lines and summing scores
   - `processLine`: Processes a single line by parsing and calculating scores
   - `calculateScore`: Calculates the score for a single game

2. **Helper Functions**:
   - `countScoringNumbers`: Counts occurrences of each scoring number
   - `scoreNumberWithCount`: Calculates points for a number based on its count
   - `baseScore`: Determines base points based on number range
   - `winningMultiplier`: Calculates multiplier based on winning number duplications

### Standard Implementation

The standard implementation builds the solution using modular components:

```haskell
countScore :: String -> Int
countScore = sum . map processLine . lines

calculateScore :: [Int] -> [Int] -> Int
calculateScore winningNumbers scoringNumbers = 
    sum $ map (uncurry (scoreNumberWithCount winningNumbers)) $ countScoringNumbers scoringNumbers

baseScore :: Int -> Int
baseScore n
    | n >= 1 && n <= 9   = 1
    | n >= 10 && n <= 19 = 2
    | n >= 20 && n <= 29 = 4
    | n >= 30 && n <= 39 = 8
    | otherwise          = 0
```

### Bonus Implementation

The bonus implementation leverages the power-of-2 pattern in the scoring system:

```haskell
powerOfTwo :: Int -> Int
powerOfTwo n = 2^n

baseScore :: Int -> Int
baseScore n
    | n >= 1 && n <= 9   = powerOfTwo 0  -- 2^0 = 1
    | n >= 10 && n <= 19 = powerOfTwo 1  -- 2^1 = 2
    | n >= 20 && n <= 29 = powerOfTwo 2  -- 2^2 = 4
    | n >= 30 && n <= 39 = powerOfTwo 3  -- 2^3 = 8
    | otherwise          = 0
```

This approach makes the code more maintainable and demonstrates the mathematical pattern in the scoring system:
- All base scores are powers of 2: 1, 2, 4, 8
- All multipliers are powers of 2: 1, 2, 4
- Each additional occurrence doubles the previous value

## Function Composition

The implementation demonstrates functional programming principles:

1. **Composition with `.`**:
   ```haskell
   countScore = sum . map processLine . lines
   ```

2. **List Comprehension**:
   ```haskell
   [(n, countOccurrences n nums) | n <- unique]
   ```

3. **Higher-Order Functions**:
   ```haskell
   map (uncurry (scoreNumberWithCount winningNumbers)) $ countScoringNumbers scoringNumbers
   ```

## Testing

Tests are implemented using DocTest and can be run with:

```bash
stack test
```

Each function includes doctests to verify its behavior with different inputs.


# Galactic Greetings: Decoding Extraterrestrial Messages

This Haskell project demonstrates advanced functional programming techniques for error handling and function composition. It implements a system that decodes messages from an extraterrestrial civilization by processing sequences of integers according to specific validation rules.

## Project Overview

The project decodes alien transmissions by validating a sequence of numbers against three criteria:
1. The minimum and maximum values must be unique in the sequence
2. The sum of the minimum and maximum must be even
3. The message is the count of occurrences of the value (min + max) / 2 in the sequence

The implementation demonstrates:
- Monadic error handling with `Maybe` and `Either`
- Function composition using bind (`>>=`) and applicative operators
- Pure functional programming without do-notation

## Prerequisites

- [Haskell](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Setup and Building

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd lab06
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

The program takes input from stdin, processes it according to the rules, and outputs the decoded message or an error:

```bash
stack run < input.txt
```

Or interactively:

```bash
stack run
5 5 5 8 1 2 3 4 9 8 2 3 4
[Ctrl+D to send EOF]
```

## Example Inputs and Expected Outputs

### Valid Message

Input:
```
5 5 5 8 1 2 3 4 9 8 2 3 4
```

Output:
```
* Minimum number: 1 (unique!)
* Maximum number: 9 (unique!)
* Sum: 10 (even and divisible by 2)
* Min + Max divided by 2: 5 (magic number!)
* Occurrences of 5: 3 (our cosmic message!)
* The message is 3!
```

### Invalid: Non-unique Minimum

Input:
```
5 5 5 8 1 2 3 4 9 8 2 3 4 1
```

Output:
```
Communication interference detected: minimum number not Unique
```

### Invalid: Non-unique Maximum

Input:
```
5 5 5 8 1 2 3 4 9 8 2 3 4 9
```

Output:
```
Communication interference detected: maximum number not Unique
```

### Invalid: Sum Not Even

Input:
```
5 5 5 1 2 3 4 8 2 3
```

Output:
```
Communication interference detected: midPoint not even
```

## Implementation Details

### Key Components

1. **Basic Functions**:
   - `parseInput`: Converts space-separated string into list of integers
   - `findMin`/`findMax`: Find minimum and maximum values in a list
   - `isUnique`: Checks if a value appears exactly once in a list
   - `isEven`: Checks if a number is even
   - `midpoint`: Calculates the average of two numbers
   - `countOccurrences`: Counts occurrences of a value in a list

2. **Validation Functions**:
   - `validateMinUnique`: Ensures minimum value appears only once
   - `validateMaxUnique`: Ensures maximum value appears only once
   - `validateSumEven`: Ensures sum of min and max is even
   - Enhanced variants with `Either` return type and error messages

3. **Message Decoding**:
   - `decodeMessage`: Uses `Maybe` for error handling (just reports success/failure)
   - `decodeMessageImproved`: Uses `Either` to provide detailed error messages

### Function Composition

The core of the implementation uses function composition with the bind operator (`>>=`):

```haskell
decodeMessage :: String -> Maybe Int
decodeMessage msg = 
    let nums = parseInput msg
    in validateMinUnique nums >>= 
       validateMaxUnique >>= 
       validateSumEven >>= 
       return . computeMessage
```

The improved version with detailed error messages:

```haskell
decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg = 
    let nums = parseInput msg
    in validateMinUniqueE nums >>= 
       validateMaxUniqueE >>= 
       validateSumEvenE >>= 
       Right . computeMessage
```

## Error Handling Approaches

1. **Using Maybe**:
   - Simple success/failure indication
   - No information about what caused the failure
   - Lightweight and minimal

2. **Using Either**:
   - Provides detailed error messages
   - Allows tracking which validation step failed
   - More informative for users

## Testing

Tests are implemented using DocTest and can be run with:

```bash
stack test
```

The tests verify:
- Individual component functions
- Complete message decoding with valid inputs
- Error handling with various invalid inputs


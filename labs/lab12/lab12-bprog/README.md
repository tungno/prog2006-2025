# BPROG Interpreter

A stack-based programming language interpreter implemented in Haskell. BPROG follows the design of languages like Forth and Factor, where operations manipulate a global stack.

## Quick Start

```bash
# Build the project
stack build

# Run the tests (all should pass)
stack test
./test-d-level.sh
./test-c-level.sh
```

## Ways to Run BPROG

### 1. REPL Mode (Most Reliable)

```bash
# Start the REPL
stack exec bprog

# Example session
> 1 2 +
Stack: [3]
> dup *
Stack: [9]
> stack        # Show the stack contents
Stack: [9]
> quit         # Exit the REPL
```

REPL commands:
- `stack` - Display current stack contents
- `help` - Show available commands
- `quit` - Exit the REPL

### 2. Direct File Execution (Limited Functionality)

```bash
# This approach works for simple operations but has limitations with complex constructs
stack exec bprog working-examples/arithmetic-examples.bprog
```

**Note:** When running files directly, you may see errors like "Expected a number" for complex programs. This is a known limitation.

### 3. Piping Examples to REPL (Recommended for Examples)

```bash
# This is the most reliable way to run example files
(cat working-examples/arithmetic-examples.bprog | grep -v "^#"; echo "stack"; echo "quit") | stack exec bprog

# Or for specific commands
(echo "1 2 +"; echo "stack"; echo "quit") | stack exec bprog
```

## Example Commands

Here are copy-paste ready commands to test BPROG's features:

### Arithmetic

```bash
# Test basic addition
(echo "1 2 +"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [3]

# Test multiplication
(echo "3 4 *"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [12]

# Test square of a number
(echo "5 dup *"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [25]
```

### List Operations

```bash
# Get list length
(echo "[ 1 2 3 ] length"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [3]

# Add element to front of list
(echo "1 [ 2 3 ] cons"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [[1,2,3]]
```

### Boolean Operations

```bash
# Test logical AND
(echo "True False &&"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [False]

# Test comparison
(echo "10 5 >"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [True]
```

### Variable Assignment

```bash
# Assign and retrieve value
(echo "x 42 := x"; echo "stack"; echo "quit") | stack exec bprog
# Output: Stack: [42]
```

## Working Example Files

The `working-examples/` directory contains tested example files:

| File | Description | Command to Run |
|------|-------------|----------------|
| `arithmetic-examples.bprog` | Basic arithmetic operations | `(echo "1 2 +"; echo "stack"; echo "quit") \| stack exec bprog` |
| `stack-examples.bprog` | Stack manipulation operations | `(echo "5 dup"; echo "stack"; echo "quit") \| stack exec bprog` |
| `list-examples.bprog` | List operations | `(echo "[ 1 2 3 ] length"; echo "stack"; echo "quit") \| stack exec bprog` |
| `boolean-examples.bprog` | Logical operations | `(echo "True False &&"; echo "stack"; echo "quit") \| stack exec bprog` |
| `variable-examples.bprog` | Variable usage | `(echo "x 42 := x"; echo "stack"; echo "quit") \| stack exec bprog` |

## Supported Features

| Feature Category | Operations |
|------------------|------------|
| **Data Types** | Integer, Float, Boolean, String, List, Quotation, Symbol |
| **Stack Operations** | `dup`, `swap`, `pop` |
| **Arithmetic** | `+`, `-`, `*`, `/`, `div` |
| **Logic** | `&&`, `||`, `not` |
| **Comparison** | `<`, `>`, `==` |
| **List Operations** | `head`, `tail`, `empty`, `length`, `cons`, `append` |
| **Higher-order Functions** | `map`, `foldl`, `each` |
| **Control Flow** | `if`, `loop`, `times` |
| **Variables & Functions** | `:=`, `fun` |

## Known Limitations

1. **File Execution**: Running programs directly from files via `stack exec bprog examples/file.bprog` has limited functionality for complex operations
2. **Control Flow**: Nested control structures may not work correctly in file mode
3. **Variable Scope**: Nested scopes might not behave as expected in complex programs

## Tips for Success

1. **Use REPL Mode**: The most reliable way to use BPROG
2. **Test One Line at a Time**: Enter commands sequentially in the REPL
3. **Monitor the Stack**: Use the `stack` command frequently 
4. **Use the Test Scripts**: The test-d-level.sh and test-c-level.sh scripts demonstrate working features

## Testing & Verification

The implementation successfully passes all required tests:

```bash
# All of these tests pass successfully
stack test
./test-d-level.sh
./test-c-level.sh
```

This demonstrates that the core functionality is correctly implemented, even though the direct file execution has some limitations.
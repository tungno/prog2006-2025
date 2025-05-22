# Rust Data Processing: Students and Birds

This Rust project demonstrates fundamental string manipulation and data processing techniques through three main tasks: string reversal, oldest student counting, and bird frequency analysis.

## Project Overview

The project consists of three distinct tasks:

1. **String Reversal**: Implementing a custom string reversal function without using Rust's built-in `rev()` method
2. **Oldest Students Counter**: Analyzing a dataset to find and count the oldest students
3. **Bird Frequency Analysis**: Finding the most frequently observed birds in a dataset, with various tie-breaking strategies

## Prerequisites

- [Rust](https://www.rust-lang.org/tools/install) (2021 edition or newer)
- [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html) (comes with Rust)

## Building and Running

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd lab08
   ```

2. Build the project:
   ```bash
   cargo build
   ```

3. Run the project:
   ```bash
   cargo run --release
   ```

4. Run tests:
   ```bash
   cargo test
   ```

## Tasks in Detail

### 1. String Reversal

The `reverse` function takes a string slice and returns a new string with the characters in reverse order. This is implemented manually without using Rust's built-in `rev()` method.

```rust
fn reverse(input: &str) -> String {
    let chars: Vec<char> = input.chars().collect();
    let mut reversed = String::with_capacity(chars.len());
    let mut idx = chars.len();
    while idx > 0 {
        idx -= 1;
        reversed.push(chars[idx]);
    }
    reversed
}
```

**Example Usage:**
```rust
let original = "Hello, world!";
let reversed = reverse(original);  // Returns "!dlrow ,olleH"
```

### 2. Oldest Students Counter

The `count_oldest_students` function analyzes a dataset containing student information and counts how many students share the maximum age.

```rust
fn count_oldest_students(data: &str) -> u64 {
    let mut max_age = 0;
    let mut oldest_count = 0;

    for line in data.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 3 {
            continue;
        }

        let age: u64 = match parts[2].parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        if age > max_age {
            max_age = age;
            oldest_count = 1;
        } else if age == max_age {
            oldest_count += 1;
        }
    }

    oldest_count
}
```

**Example Input:**
```
Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21
```

**Expected Output:** 3 (Cooper, Chains, and Chaplin are all 25 years old)

### 3. Bird Frequency Analysis

This task encompasses three related functions:

#### a) `find_most_frequent_bird_count`

Returns the count of the most frequent bird in the dataset.

```rust
fn find_most_frequent_bird_count(birds: &Vec<&str>) -> u64 {
    use std::collections::HashMap;
    let mut freq_map = HashMap::new();
    let mut max_count = 0;

    for &bird in birds {
        let count = freq_map.entry(bird).or_insert(0);
        *count += 1;
        if *count > max_count {
            max_count = *count;
        }
    }

    max_count
}
```

#### b) `find_most_frequent_bird_no_order`

Returns one of the most frequent birds (if there's a tie, any of them may be returned).

```rust
fn find_most_frequent_bird_no_order<'a>(birds: &[&'a str]) -> Option<&'a str> {
    use std::collections::HashMap;

    if birds.is_empty() {
        return None;
    }

    let mut freq_map = HashMap::new();
    for &bird in birds {
        let count = freq_map.entry(bird).or_insert(0);
        *count += 1;
    }

    let mut max_bird = None;
    let mut max_count = 0;
    for (&bird, &count) in freq_map.iter() {
        if count > max_count {
            max_count = count;
            max_bird = Some(bird);
        }
    }

    max_bird
}
```

#### c) `find_most_frequent_bird`

Returns the most frequent bird, but in case of a tie, returns the one that was observed first.

```rust
fn find_most_frequent_bird<'a>(birds: &[&'a str]) -> Option<&'a str> {
    use std::collections::HashMap;

    if birds.is_empty() {
        return None;
    }

    // Count frequencies
    let mut freq_map = HashMap::new();
    for &bird in birds {
        *freq_map.entry(bird).or_insert(0) += 1;
    }

    // Identify the maximum frequency
    let max_count = freq_map.values().copied().max().unwrap_or(0);

    // Collect all birds that have that max_count
    let mut candidates: Vec<&str> = freq_map
        .iter()
        .filter_map(|(&bird, &count)| {
            if count == max_count {
                Some(bird)
            } else {
                None
            }
        })
        .collect();

    // Sort them alphabetically and return the first
    candidates.sort();
    Some(candidates[0])
}
```

## Implementation Notes

- The project uses standard Rust features like `HashMap` for frequency counting
- Error handling is implemented with `match` expressions for parsing
- Iterator methods (`map`, `filter_map`, etc.) are used for clean data processing
- For each task, the code focuses on single-pass algorithms for efficiency

## Testing

The project includes comprehensive tests for each function:

- `test_reverse`: Tests the string reversal function with various inputs
- `test_oldest_students_count`: Tests the oldest student counting with a sample dataset
- `test_find_most_frequent_bird_count`: Tests finding the count of the most frequent bird
- `test_find_most_frequent_bird_with_order`: Tests finding the most frequent bird with ordering
- `test_find_most_frequent_bird_no_order`: Tests finding the most frequent bird without ordering

## Performance Considerations

- The implementations prioritize single-pass algorithms for O(n) time complexity
- Memory usage is optimized by using iterators and direct parsing where possible
- HashMap operations provide O(1) lookups for efficient frequency counting


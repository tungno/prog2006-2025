fn main() {
    test_reverse();

    test_oldest_students_count();

    test_find_most_frequent_bird_count();
    test_find_most_frequent_bird(find_most_frequent_bird);
    test_find_most_frequent_bird(find_most_frequent_bird_no_order);

    test_find_most_frequent_bird_with_order();
    test_find_most_frequent_bird_no_order();

    // Test the count_oldest_students function
    test_oldest_students_count();
    
    // Additional test with explanation for oldest students
    explain_oldest_students_count();
}

fn reverse(input: &str) -> String {
    // Implement the custom reverse without using .rev() on an iterator.
    // Example approach: store chars in a vector, then iterate backwards manually.
    let chars: Vec<char> = input.chars().collect();
    let mut reversed = String::with_capacity(chars.len());
    let mut idx = chars.len();
    while idx > 0 {
        idx -= 1;
        reversed.push(chars[idx]);
    }
    reversed
}

fn count_oldest_students(data: &str) -> u64 {
    // We'll parse each line, splitting into [first_name, last_name, age].
    // Track the maximum age and how many times that maximum has appeared.
    let mut max_age = 0;
    let mut oldest_count = 0;

    for line in data.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        // We expect each line to have at least 3 parts: first_name, last_name, age
        if parts.len() < 3 {
            continue; // skip for next lines
        }

        // Attempt to parse the age
        let age: u64 = match parts[2].parse() {
            Ok(num) => num,  // if the parse success Ok(num)
            Err(_) => continue, // skip if it's not a valid integer, _ underscore ignores the specific error details
        };

        if age > max_age {
            max_age = age;
            oldest_count = 1;  // reset the count to 1, since this is the first student with this new maximum age.
        } else if age == max_age {
            oldest_count += 1;
        }
    }

    oldest_count
}

fn find_most_frequent_bird_count(birds: &Vec<&str>) -> u64 {
    // Count frequencies in one pass using a HashMap from standard library
    use std::collections::HashMap;
    let mut freq_map = HashMap::new();
    let mut max_count = 0;

    for &bird in birds {
        let count = freq_map.entry(bird).or_insert(0);
        *count += 1;  // * is used to dereference the mutable reference to modify the actual value in the map.
        if *count > max_count {
            max_count = *count;
        }
    }

    max_count
}

fn find_most_frequent_bird_no_order<'a>(birds: &[&'a str]) -> Option<&'a str> {
    // If multiple birds are equally frequent, we can return any one of them.
    // That means we do not care about the order they appeared in.
    use std::collections::HashMap;

    if birds.is_empty() {
        return None;
    }

    let mut freq_map = HashMap::new();
    // First pass: count frequencies
    for &bird in birds {
        let count = freq_map.entry(bird).or_insert(0);
        *count += 1;
    }

    // Second pass: find the largest frequency.
    // Because HashMap iteration order is not guaranteed, in a tie,
    // we might pick any bird with the highest frequency.
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

fn find_most_frequent_bird<'a>(birds: &[&'a str]) -> Option<&'a str> {  // <'a> syntax indicates a lifetime parameter, 
    use std::collections::HashMap;  // imports the HashMap data structure from thee standard lib

    if birds.is_empty() {
        return None;
    }

    // 1) Count frequencies
    let mut freq_map = HashMap::new();
    for &bird in birds {
        *freq_map.entry(bird).or_insert(0) += 1;
    }

    // 2) Identify the maximum frequency
    let max_count = freq_map.values().copied().max().unwrap_or(0); // .unwrap_or(0) default to 0 if  there are no values

    // 3) Collect all birds that have that max_count
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

    // 4) Sort them alphabetically and return the first
    candidates.sort();
    Some(candidates[0])
}



fn explain_oldest_students_count() {
    // Use the same data as in test_oldest_students_count()
    let data = r#"Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21"#;

    let mut max_age = 0;
    let mut oldest_last_names = Vec::new();

    for line in data.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 3 {
            continue; // skip malformed lines
        }

        // Attempt to parse the age
        let age: u64 = match parts[2].parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        // If we found a new max age, reset the list. Otherwise, if it ties the max, add to the list.
        if age > max_age {
            max_age = age;
            oldest_last_names.clear();
            oldest_last_names.push(parts[1].to_string()); // last name is parts[1]
        } else if age == max_age {
            oldest_last_names.push(parts[1].to_string());
        }
    }

    // Join the last names with commas
    let count = oldest_last_names.len();
    let joined_names = oldest_last_names.join(", ");

    println!(
        "The oldest people are {}, thus the count of oldest students is {}.",
        joined_names, count
    );
}


/////////////////////////////// Tests //////////////////////////////////
// The test functions given by the skeleton code:

fn test_reverse() {
    let data = [
        ("", ""),
        ("a", "a"),
        ("Hello", "olleH"),
        ("World", "dlroW"),
        ("1234567890", "0987654321"),
        ("123456789", "987654321"),
        ("This is my string", "gnirts ym si sihT"),
        ("This\tis my\n string", "gnirts \nym si\tsihT"),
    ];
    for (input, expected) in data {
        let output = reverse(input);
        assert_eq!(output, expected);
    }
    println!("test_reverse() passed.");
}

fn test_oldest_students_count() {
    let data = r#"Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21"#;
    let output = count_oldest_students(data);
    assert_eq!(output, 3);
    println!("test_oldest_students_count() passed.");
}

fn test_find_most_frequent_bird_count() {
    let data = [
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1"], 3),
        (
            vec!["zz", "a1", "zz", "bz2", "a3", "a1", "bz2", "a1", "a1"],
            4,
        ),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1"], 5),
        (
            vec!["zz", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1"],
            6,
        ),
        (
            vec![
                "zz", "bz2", "bz2", "a1", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1",
            ],
            7,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            8,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            9,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            10,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1",
            ],
            11,
        ),
        (
            vec![
                "bz2", "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1", "a1", "a1",
            ],
            12,
        ),
    ];
    for (input, expected) in data {
        let output = find_most_frequent_bird_count(&input);
        assert_eq!(output, expected);
    }
    println!("test_find_most_frequent_bird_count() passed.");
}

fn test_find_most_frequent_bird(f: for<'a> fn(&[&'a str]) -> Option<&'a str>) {
    let data = [
        (vec![], None),
        (vec!["a1"], Some("a1")),
        (
            vec!["bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1"],
            Some("a1"),
        ),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1"], Some("a1")),
        (
            vec!["bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1"],
            Some("a1"),
        ),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1"],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "bz2", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
    ];

    for (input, expected) in data {
        let output = f(&input);  // f is a parameter passed to the test function,
        assert_eq!(output, expected);
    }
    println!("test_find_most_frequent_bird(...) passed for function.");
}

fn test_find_most_frequent_bird_no_order() {
    let data = [
        (vec![], vec![None]),
        (vec!["a1"], vec![Some("a1")]),
        (
            vec!["a1", "a2", "a3"],
            vec![Some("a1"), Some("a2"), Some("a3")],
        ),
        (
            vec!["a1", "a2", "a3", "a2", "a3", "a1"],
            vec![Some("a1"), Some("a2"), Some("a3")],
        ),
        (
            vec!["a2", "a1", "a1", "a2", "a2", "a3", "a1"],
            vec![Some("a2"), Some("a1")],
        ),
        (
            vec!["a1", "a2", "a2", "a1", "a2", "a1"],
            vec![Some("a1"), Some("a2")],
        ),
        (
            vec!["a1", "a2", "a3", "a3", "a3", "a2", "a2"],
            vec![Some("a2"), Some("a3")],
        ),
    ];

    for (input, expected) in data {
        let output = find_most_frequent_bird_no_order(&input);
        assert!(expected.contains(&output));
    }
    println!("test_find_most_frequent_bird_no_order() passed.");
}

fn test_find_most_frequent_bird_with_order() {
    let data = [
        (vec![], None),
        (vec!["a1"], Some("a1")),
        (vec!["a1", "a2", "a3"], Some("a1")),
        (vec!["a1", "a2", "a3", "a2", "a3", "a1"], Some("a1")),
        (vec!["a2", "a1", "a1", "a2", "a2", "a3"], Some("a2")),
        (vec!["a1", "a2", "a2", "a1", "a2", "a1"], Some("a1")),
        (vec!["a1", "a2", "a3", "a3", "a3", "a2", "a2"], Some("a2")),
    ];

    for (input, expected) in data {
        let output = find_most_frequent_bird(&input);
        assert_eq!(output, expected);
    }
    println!("test_find_most_frequent_bird_with_order() passed.");
}
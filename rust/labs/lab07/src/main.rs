// Lab 07
use std::collections::HashMap;
fn main() {
    test_acronym();
    test_reverse();

    test_find_most_frequent_bird_count();
    test_find_most_frequent_bird(find_most_frequent_bird);

    // * This would fail becuase the test cases are dependent on the order of the most frequent bird appearance and the function does not take that into account (find_most_frequent_bird_with_no_order)
    // test_find_most_frequent_bird(find_most_frequent_bird_no_order);

    test_find_most_frequent_bird_with_order();
    test_find_most_frequent_bird_no_order();

    println!("\n\n[ALL TESTS PASSED]")
}

fn reverse(input: &str) -> String {
    let mut res = String::new();
    let mut chars: Vec<char> = input.chars().collect();

    while let Some(c) = chars.pop() {
        res.push(c);
    }
    res
}

fn acronym(phrase: &str) -> String {
    phrase
        .split(&[' ', '-', '_', '|', '\t', '\n'])
        .flat_map(|word| {
            if word.chars().filter(|c| c.is_uppercase()).count() > 1 {
                word.split(|c: char| !c.is_uppercase()).collect()
            } else {
                vec![&word[0..1]]
            }
        })
        .filter(|c| !c.is_empty())
        .map(|txt| &txt[0..1])
        .collect::<String>()
        .to_uppercase()
}

fn find_most_frequent_bird_count(birds: &[&str]) -> u64 {
    let mut bird_counts: HashMap<&str, u64> = HashMap::new();

    birds
        .iter()
        .for_each(|bird| *bird_counts.entry(bird).or_insert(0) += 1);

    *bird_counts
        .iter()
        .max_by_key(|&(_, count)| count)
        .unwrap()
        .1 // unwrap (which is safe in this case) will return a tuple
}

fn find_most_frequent_bird_no_order<'a>(birds: &[&'a str]) -> Option<&'a str> {
    if birds.is_empty() {
        return None;
    }

    let mut bird_counts: HashMap<&str, u64> = HashMap::new();

    for bird in birds.iter() {
        bird_counts.entry(bird).and_modify(|c| *c += 1).or_insert(1);
    }

    Some(
        *bird_counts
            .iter()
            .max_by_key(|&(_, count)| count)
            .unwrap() // unwrap (which is safe in this case) will return a tuple
            .0,
    )
}

fn find_most_frequent_bird<'a>(birds: &[&'a str]) -> Option<&'a str> {
    if birds.is_empty() {
        return None;
    }

    let mut bird_counts: HashMap<&str, u64> = HashMap::new();
    let mut max: (&str, u64) = ("", 0); // (name, count)
    let mut index_of_bird;
    let mut prev_index = 0;

    for bird in birds.iter() {
        let count = bird_counts.entry(bird).and_modify(|c| *c += 1).or_insert(1);
        index_of_bird = birds.iter().position(|b| b == bird).unwrap();

        // Only update if the index of current bird is less than previous index
        if *count > max.1 || index_of_bird < prev_index {
            max.1 = *count;
            max.0 = bird;
        }
        prev_index = index_of_bird;
    }

    Some(max.0)
}

// Run the tests with `cargo run --release` to see if everything worked.

fn test_acronym() {
    let data = vec![
        (
            "Lecturer's like to use acronyms in their lectures",
            "LLTUAITL",
        ),
        (
            "Lecturer's like-to_use|acronyms in their\tlectures",
            "LLTUAITL",
        ),
        ("Portable Network Graphics", "PNG"),
        ("GNU Image Manipulation Program", "GIMP"),
        ("GNU Image Manipulation Program", "GIMP"),
        ("Rolling On The Floor Laughing So Hard", "ROTFLSH"),
        ("Ruby on Rails", "ROR"),
        ("HyperText Markup Language", "HTML"),
        ("First In, First Out", "FIFO"),
        ("PHP: Hypertext Preprocessor", "PHP"),
        ("PNG: Network Graphics", "PNG"),
        ("Make IT easy", "MIE"),
        ("Make I:T easy", "MITE"),
        ("Make I'T easy", "MITE"),
        ("Complementary metal-oxide semiconductor", "CMOS"),
        ("Complementary:metal-oxide semiconductor", "COS"),
    ];
    for (input, expected) in data {
        let output = acronym(input);

        print!("[TESTING]: {} -> {}", input, output);
        assert_eq!(output, expected);
        println!("\t[OK]");
    }
}

fn test_reverse() {
    let data = [
        ("", ""),
        ("a", "a"), // edge cases
        ("Hello", "olleH"),
        ("World", "dlroW"), // Hellow World, of course :)
        ("1234567890", "0987654321"),
        ("123456789", "987654321"),
        ("This is my string", "gnirts ym si sihT"),
        ("This\tis my\n string", "gnirts \nym si\tsihT"), // with tabs and newlines
    ];
    for (input, expected) in data {
        let output = reverse(input);

        print!("[TESTING]: {} -> {}", input, output);
        assert_eq!(output, expected);
        println!("\t[OK]");
    }
}

fn test_find_most_frequent_bird_count() {
    let data = [
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1"], 3_u64),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1"], 4),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1"], 5),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1"],
            6,
        ),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1"],
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
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1",
            ],
            12,
        ),
    ];
    for (input, expected) in data {
        let output = find_most_frequent_bird_count(&input);
        assert_eq!(output, expected);
    }
}

// Normal test cases that are independent on the order of the most frequent bird appearance
// in the data log.
fn test_find_most_frequent_bird(f: for<'a> fn(&[&'a str]) -> Option<&'a str>) {
    let data = [
        (vec![], None),           // edge case
        (vec!["a1"], Some("a1")), // edge case
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1"], Some("a1")),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1"], Some("a1")),
        (
            vec!["a5", "bz2", "a3", "a3", "bz2", "a3", "a5", "a5"],
            Some("a5"),
        ),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1"],
            Some("a1"),
        ),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1"],
            Some("a1"),
        ),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1"],
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
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
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
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "a5", "bz2", "a3", "a5", "bz2", "a5", "a5", "a5", "a5", "a5", "a5", "a5", "a5",
                "a5", "a5",
            ],
            Some("a5"),
        ),
    ];

    for (input, expected) in data {
        println!("[TESTING]: {:?}", input);
        let output = f(&input);
        assert_eq!(output, expected);
    }
}

// Test cases that are dependent on the order of the most frequent bird appearance
// in the data log. These test if the spec has been completed properly.
fn test_find_most_frequent_bird_with_order() {
    let data = [
        (vec![], None),                                         // edge case
        (vec!["a1"], Some("a1")),                               // edge case
        (vec!["a1", "a2", "a3"], Some("a1")), // edge case, must return the first most frequent
        (vec!["a1", "a2", "a3", "a1", "a2", "a3"], Some("a1")), // edge case, must return the first most frequent
        (vec!["a1", "a2", "a2", "a1", "a2", "a1"], Some("a1")),
        (vec!["a1", "a2", "a3", "a3", "a3", "a2", "a2"], Some("a2")), // edge case, must return the first most frequent
        (vec!["a2", "a1", "a1", "a2", "a2", "a3"], Some("a2")),
    ];

    for (input, expected) in data {
        println!("{:?}", input);
        let output = find_most_frequent_bird(&input);
        assert_eq!(output, expected);
        println!("{:?}", output)
    }
}

fn test_find_most_frequent_bird_no_order() {
    let data = [
        (vec![], vec![None]),           // edge case
        (vec!["a1"], vec![Some("a1")]), // edge case
        (
            vec!["a1", "a2", "a3"],
            vec![Some("a1"), Some("a2"), Some("a3")],
        ), // edge case, must return the first most frequent
        (
            vec!["a1", "a2", "a3", "a1", "a2", "a3"],
            vec![Some("a1"), Some("a2"), Some("a3")],
        ), // edge case, must return the first most frequent
        (vec!["a2", "a1", "a1", "a2", "a2", "a3"], vec![Some("a2")]),
        (
            vec!["a1", "a2", "a2", "a1", "a2", "a1"],
            vec![Some("a1"), Some("a2")],
        ),
        (
            vec!["a1", "a2", "a3", "a3", "a3", "a2", "a2"],
            vec![Some("a2"), Some("a3")],
        ), // edge case, must return the first most frequent
    ];

    for (input, expected) in data {
        let output = find_most_frequent_bird_no_order(&input);
        // Debugging output in case we make an error in test cases
        println!("{:?} is in {:?}", output, expected);
        assert!(expected.contains(&output));
    }
}

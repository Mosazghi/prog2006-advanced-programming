use regex::Regex;
use std::fs;

/// Read a file and return the lines as a vector of strings.
///
/// # Arguments
/// file_path - A string slice that holds the path to the file.
///
/// # Returns
/// An `Result<Vec<String>, Box<dyn std::error::Error>` that holds the lines of the file.
/// **The lines are filtered to remove empty lines.**
pub fn read_file(file_path: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    Ok(fs::read_to_string(file_path)?
        .split('\n')
        .filter(|ins| !ins.is_empty())
        .map(|s| s.to_string())
        .collect::<Vec<String>>())
}

/// Process a vector of inscriptions to get the sum of the first and last digits multiplied by 10.
///
/// # Arguments
/// inscriptions - A vector of strings that holds the inscriptions.
///  
/// # Returns
/// An `Result<u128, Box<dyn std::error::Error>` that holds the sum of all the processed inscriptions.
pub fn processor(inscriptions: Vec<String>) -> Result<u128, Box<dyn std::error::Error>> {
    // Do the processing.
    let result: Result<Vec<u128>, _> = inscriptions
        .iter()
        .map(|inscription| process_inscription(inscription.as_str()))
        .collect();

    Ok(result?.iter().sum::<u128>() % 360)
}

/// Process an inscription to get the first and last digits and multiply them.
///
/// # Arguments
/// inscription - A string slice that holds the inscription.
///
/// # Returns
/// An `Result<u128, Box<dyn std::error::Error>` that holds the result of the multiplication.
/// **The result is the first digit multiplied by 10 and added to the last digit.**
pub fn process_inscription(inscription: &str) -> Result<u128, Box<dyn std::error::Error>> {
    let digits = get_digits(inscription)?;
    // Parse the digits, regardless of whether they are words or numbers
    let parsed_digits: Vec<u128> = digits
        .iter()
        .map(|digit| parse_digit_string(digit).unwrap_or(digit.parse().unwrap_or(0)))
        .collect();

    let first = *parsed_digits.first().unwrap_or(&(0));
    let last = *parsed_digits.last().unwrap_or(&(0));

    Ok(first * 10 + last)
}
/// Get the first and last digits from an inscription.
///
/// # Arguments
/// inscription - A string slice that holds the inscription.
///
/// # Returns
/// A vector of strings that contains the first and last digits.
/// **This could be in the form of a digit (e.g. `"1"`) or a word (e.g. `"one"`).**
pub fn get_digits(inscription: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let regex_first_digit =
        Regex::new(r"[1-9]|one|two|three|four|five|six|seven|eight|nine").unwrap();
    let regex_last_digit =
        Regex::new(r".*([1-9]|one|two|three|four|five|six|seven|eight|nine)").unwrap();

    let first_match = regex_first_digit
        .find(inscription)
        .map(|mat| mat.as_str().to_string())
        .unwrap();

    let last_match = regex_last_digit
        .captures(inscription)
        .unwrap()
        .get(1)
        .unwrap()
        .as_str()
        .to_string();

    Ok(vec![first_match, last_match])
}

/// Parse a digit string to a number.
///
/// # Arguments
/// digit_string - A string slice that holds the digit string.
///
/// # Returns
/// An `Option<u128>` that holds the parsed digit.
pub fn parse_digit_string(digit_string: &str) -> Option<u128> {
    match digit_string {
        "one" => Some(1),
        "two" => Some(2),
        "three" => Some(3),
        "four" => Some(4),
        "five" => Some(5),
        "six" => Some(6),
        "seven" => Some(7),
        "eight" => Some(8),
        "nine" => Some(9),
        _ => None,
    }
}

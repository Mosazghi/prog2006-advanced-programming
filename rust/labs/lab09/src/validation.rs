use crate::guardian;
use guardian::GuardianError;

/// Custom Result type specific to this program.
#[derive(Debug)]
pub enum Result_ {
    Ok(),
    Err(Vec<String>),
}

/// Trait for combining two Result_ types.
pub trait Semigroup {
    fn combine(self, other: Self) -> Self;
}

/// Implementing the Semigroup trait for the Result_ type.
impl Semigroup for Result_ {
    fn combine(self, other: Self) -> Self {
        match (self, other) {
            (Result_::Ok(), Result_::Ok()) => Result_::Ok(),
            (Result_::Err(mut a), Result_::Err(b)) => {
                a.extend(b);
                Result_::Err(a)
            }
            (Result_::Ok(), Result_::Err(b)) => Result_::Err(b),
            (Result_::Err(a), Result_::Ok()) => Result_::Err(a),
        }
    }
}

/// Validates whether a char is capitalised or not.
/// # Arguments
/// * `char` - The char to be validated.
/// * `err_type` - The *specific* error type  to be returned if the char is not capitalised.
/// # Returns
/// * `Result_::Ok()` if the char is capitalised.
/// * `Result_::Err(vec![err_type.display().to_string()])` if the char is not capitalised.
pub fn validate_capitalised(char: &char, err_type: GuardianError) -> Result_ {
    if char.is_uppercase() {
        Result_::Ok()
    } else {
        Result_::Err(vec![err_type.display().to_string()])
    }
}

/// Validates where a string has the letter 'a' at exactly twice.
/// # Arguments
/// * `name_chars` - The name to be validated.
/// # Returns
/// * `Result_::Ok()` if the name has the letter 'a' at exactly twice.
/// * `Result_::Err(vec![GuardianError::WaterGuardianNameMissingA.display().to_string()])` if the name does not have the letter 'a' at exactly twice.
pub fn validate_missing_a(name_chars: &[char]) -> Result_ {
    if name_chars.iter().filter(|&&c| c == 'a').count() == 2 {
        Result_::Ok()
    } else {
        Result_::Err(vec![GuardianError::WaterGuardianNameMissingA
            .display()
            .to_string()])
    }
}

/// Validates where a decoded JSON value is a number or not.
///
/// # Arguments
/// * `number` - The number to be validated.
/// * `err_type` - The *specific* error type  to be returned if the number is not valid.
/// # Returns
/// * `Result_::Ok()` if the number is valid.
/// * `Result_::Err(vec![err_type.display().to_string()])` if the number is not valid.
pub fn validate_valid_numebr(number: &serde_json::Value, err_type: GuardianError) -> Result_ {
    if number.is_i64() {
        Result_::Ok()
    } else {
        Result_::Err(vec![err_type.display().to_string()])
    }
}

/// Validates whether a number is within a given range.
///
/// # Arguments
///
/// * `number` - The number to be validated.
/// * `min` - The minimum value of the range.
/// * `max` - The maximum value of the range.
/// * `err_type` - The *specific* error type  to be returned if the number is not within the range.
///
/// # Returns
///
/// * `Result_::Ok()` if the number is within the range.
/// * `Result_::Err(vec![err_type.display().to_string()])` if the number is not within the range.
pub fn validate_range_number(number: &i64, min: i64, max: i64, err_type: GuardianError) -> Result_ {
    if (min..=max).contains(number) {
        Result_::Ok()
    } else {
        Result_::Err(vec![err_type.display().to_string()])
    }
}

/// Validates whether a number is even.
/// # Arguments
/// * `number` - The number to be validated.
/// * `err_type` - The *specific* error type  to be returned if the number is not even.
/// # Returns
/// * `Result_::Ok()` if the number is even.
/// * `Result_::Err(vec![err_type.display().to_string()])` if the number is not even.
pub fn validate_even_number(number: &i64, err_type: GuardianError) -> Result_ {
    if number % 2 == 0 {
        Result_::Ok()
    } else {
        Result_::Err(vec![err_type.display().to_string()])
    }
}

/// Validates whether a string misses a dot at the end.
/// # Arguments
/// * `riddle` - The riddle to be validated.
/// # Returns
/// * `Result_::Ok()` if the riddle ends with a dot.
/// * `Result_::Err(vec![GuardianError::SkyRiddleMissingDot.display().to_string()])` if the riddle does not end with a dot.
pub fn validate_missing_dot(riddle: &[char]) -> Result_ {
    if riddle.last().unwrap_or(&' ') == &'.' {
        Result_::Ok()
    } else {
        Result_::Err(vec![GuardianError::SkyRiddleMissingDot
            .display()
            .to_string()])
    }
}

/// Validates whether a string has an even number of vowels (a, e, i, o, u).
///
/// # Arguments
/// * `riddle` - The riddle to be validated.
/// # Returns
/// * `Result_::Ok()` if the riddle has an even number of vowels.
/// * `Result_::Err(vec![GuardianError::SkyRiddleNotEven.display().to_string()])` if the riddle does not have an even number of vowels.
pub fn validate_even_vowelse(riddle: &[char]) -> Result_ {
    let vowels = ['a', 'e', 'i', 'o', 'u'];

    if riddle.iter().filter(|&c| vowels.contains(c)).count() % 2 == 0 {
        Result_::Ok()
    } else {
        Result_::Err(vec![GuardianError::SkyRiddleNotEven.display().to_string()])
    }
}

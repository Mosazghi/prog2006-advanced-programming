use crate::validation;
use serde::{Deserialize, Serialize};
use serde_json::{self, Value};
use std::collections::BTreeMap;
use std::fs;
use std::result::Result;
use validation::*;

const VALID_RESULT: &str = "VALID";

/// Struct representing a guardian.
#[derive(Serialize, Deserialize, Debug)]
pub struct Guardian {
    uid: i64,
    #[serde(rename = "greenstoneWeight")]
    greenstone_weight: i64,
    #[serde(rename = "waterGuardian")]
    water_guardian: String,
    #[serde(rename = "vulcanicCave")]
    vulcanic_cave: i64,
    #[serde(rename = "skyRiddle")]
    sky_riddle: String,
}

/// Enum representing the possible errors of the guardian.
#[derive(Deserialize, Debug, Serialize)]
pub enum GuardianError {
    GreenstoneWeightNotNumber,
    GreenstoneWeightOutOfRange,
    GreenstoneWeightNotEven,
    WaterGuardianNameOutOfRange,
    WaterGuardianNameNotCapitalised,
    WaterGuardianNameMissingA,
    VulcanicCaveTemperatureOutOfRange,
    VulcanicCaveTemperatureNotEven,
    VulcanicCaveTemperatureNotNumber,
    SkyRiddleNotCapitalised,
    SkyRiddleMissingDot,
    SkyRiddleNotEven,
}

/// Implementing the `display` method for the GuardianError enum which returns a string representation of the error.
impl GuardianError {
    pub fn display(&self) -> &str {
        match *self {
            GuardianError::GreenstoneWeightNotNumber => "Greenstone weight: not a number",
            GuardianError::GreenstoneWeightOutOfRange => "Greenstone weight: outside the range",
            GuardianError::GreenstoneWeightNotEven => "Greenstone weight: not even",
            GuardianError::WaterGuardianNameOutOfRange => "Water guardian name: outside the range",
            GuardianError::WaterGuardianNameNotCapitalised => {
                "Water guardian name: not capitalised"
            }

            GuardianError::WaterGuardianNameMissingA => "Water guardian name: missing a",
            GuardianError::VulcanicCaveTemperatureOutOfRange => {
                "Vulcanic cave temperature: outside the range"
            }

            GuardianError::VulcanicCaveTemperatureNotEven => "Vulcanic cave temperature: not even",
            GuardianError::VulcanicCaveTemperatureNotNumber => {
                "Vulcanic cave temperature: not a number"
            }

            GuardianError::SkyRiddleNotCapitalised => "Sky riddle: not capitalised",
            GuardianError::SkyRiddleMissingDot => "Sky riddle: missing dot",
            GuardianError::SkyRiddleNotEven => "Sky riddle: not even",
        }
    }
}

/// Reads a JSON file and returns a vector of guardians.
/// # Arguments
/// * `file_path` - The path to the JSON file.
/// # Returns
/// * A vector of guardians.
/// # Errors
/// * If the file does not exist or the JSON is not valid.
pub fn read_json_to_guardians(file_path: &str) -> Result<Vec<Guardian>, serde_json::Error> {
    let data: String =
        fs::read_to_string(file_path).expect("Coultn't read the file. Check if it exists.");
    let guardians: Value = serde_json::from_str(&data)?;
    let guardians: Vec<Guardian> = guardians
        .as_array()
        .unwrap()
        .iter()
        .map(|g: &Value| {
            serde_json::from_value(g.clone()).unwrap_or_else(|_| {
                println!("\n[Error parsing guardian]: {g:#?} \n\n Hint: are the numbers valid? ");
                std::process::exit(1);
            })
        })
        .collect();
    Ok(guardians)
}

/// Validates the 'Water' gem of the guardian.
fn validate_water_guardian(guardian: &Guardian) -> Result_ {
    let name_chars: &Vec<char> = &guardian.water_guardian.chars().collect();

    validate_range_number(
        &(name_chars.len() as i64),
        3,
        15,
        GuardianError::WaterGuardianNameOutOfRange,
    )
    .combine(validate_capitalised(
        name_chars.first().unwrap_or(&'A'),
        GuardianError::WaterGuardianNameNotCapitalised,
    ))
    .combine(validate_missing_a(name_chars))
}

/// Validates the 'Earth' gem of the guardian.
fn validate_earth_guardian(guardian: &Guardian) -> Result_ {
    let weight = &guardian.greenstone_weight;

    // Depending if the JSON deserializes the number as a string or not, we might need to validate it as a number or not.
    // validate_valid_numebr(weight, GuardianError::GreenstoneWeightNotNumber)
    //     .combine

    (validate_range_number(weight, 13, 113, GuardianError::GreenstoneWeightOutOfRange)).combine(
        validate_even_number(weight, GuardianError::GreenstoneWeightNotEven),
    )
}

/// Validates the 'Fire' gem of the guardian.
fn validate_fire_guardian(guardian: &Guardian) -> Result_ {
    let temperature = &guardian.vulcanic_cave;
    // Depending if the JSON deserializes the number as a string or not, we might need to validate it as a number or not.

    // validate_valid_numebr(temperature, GuardianError::VulcanicCaveTemperatureNotNumber)
    //     .combine

    validate_range_number(
        temperature,
        400,
        700,
        GuardianError::VulcanicCaveTemperatureOutOfRange,
    )
    .combine(validate_even_number(
        temperature,
        GuardianError::VulcanicCaveTemperatureNotEven,
    ))
}

/// Validates the 'Air' gem of the guardian.
fn validate_missing_air_guardian(guardian: &Guardian) -> Result_ {
    let name_chars: &Vec<char> = &guardian.sky_riddle.chars().collect();

    validate_even_vowelse(name_chars)
        .combine(validate_capitalised(
            name_chars.first().unwrap_or(&'A'),
            GuardianError::SkyRiddleNotCapitalised,
        ))
        .combine(validate_missing_dot(name_chars))
}

/// Validates the guardian's gems and returns a string with the errors.
fn validate_guardian(guardian: &Guardian) -> Result_ {
    validate_water_guardian(guardian)
        .combine(validate_earth_guardian(guardian))
        .combine(validate_fire_guardian(guardian))
        .combine(validate_missing_air_guardian(guardian))
}

/// Validates the guardians and returns a hashmap with the validation results.
/// # Arguments
/// * `guardians` - The guardians to be validated.
/// # Returns
/// * A hashmap with the validation results.
pub fn validate_and_return_errors(guardians: &[Guardian]) -> BTreeMap<i64, String> {
    let mut output: BTreeMap<i64, String> = BTreeMap::new();

    guardians.iter().for_each(|g| {
        let result = validate_guardian(g);
        output.insert(
            g.uid,
            match result {
                Result_::Ok() => VALID_RESULT.to_string(),
                Result_::Err(errors) => errors.join(", "),
            },
        );
    });

    output
}

/// Writes the guardians to a file.
/// NOTE: Using BTreemap to keep it sorted such that it's easier to compare the expected output.
///
/// # Arguments
/// * `guardians` - The guardians to be written to the file.
/// * `file_path` - The path to the file.
/// # Returns
/// * `Result_::Ok()` if the file was written successfully.
/// * `Result_::Err(std::io::Error)` if the file could not be written.
pub fn write_guardians_to_file(
    guardians: BTreeMap<i64, String>,
    file_path: &str,
) -> Result<(), std::io::Error> {
    let mut output: String = String::new();
    for (uid, result) in guardians {
        output.push_str(&format!("{}: {}\n", uid, result));
    }
    fs::write(file_path, output)?;
    Ok(())
}

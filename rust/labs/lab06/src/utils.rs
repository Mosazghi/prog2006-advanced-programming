use std::fs;
use std::fs::File;
use std::io::{self, BufRead};
pub const MULTIPLIER: u128 = 100000000000;

/// This function is used to read from a file and parse the values to a vector
/// # Arguments
/// * `file_path` - A string slice
/// # Returns
/// * A vector of u128 values
/// # Panics
/// * If the file cannot be opened
pub fn read_from_file_parse_to_vector(file_path: &str) -> Vec<u128> {
    let file = File::open(file_path).expect("Unable to open file!");
    let reader = io::BufReader::new(file);

    let mut numbers: Vec<u128> = Vec::new();

    // Here we are reading the file line by line and parsing the values to u128
    // we are splitting the line by '.' and then parsing the whole part and the fractional part
    // we are multiplying the whole part by 100 and then adding the fractional part
    for line in reader.lines() {
        let line = line.expect("Unable to read line!");
        let parts: Vec<&str> = line.split('.').collect();
        let whole_part: u128 = parts[0].parse::<u128>().unwrap() * 100;
        let fractional_part: u128 = {
            let frac = parts[1].to_string();
            frac[0..2].parse::<u128>().unwrap()
        };
        numbers.push(whole_part + fractional_part);
    }
    println!("Read from '{file_path}' successfully");
    numbers
}

/// This function is used to write to a file
/// # Arguments
/// * `file_path` - A string slice
/// * `data` - A vector of u128 values
/// # Returns
/// * A vector of u128 values
/// # Panics
/// * If the file cannot be written
pub fn write_to_file(file_path: &str, data: Vec<u128>) {
    let data = data
        .iter()
        .map(|x: &u128| format!("{:.2}", *x as f64 / MULTIPLIER as f64))
        .collect::<Vec<String>>()
        .join("\n");
    fs::write(file_path, data).expect("Unable to write file");
    println!("'{file_path}' written successfully");
}

/// This function is used to calculate the earnings
/// # Arguments
/// * `taxes` - A vector of u128 values
/// # Returns
/// * A vector of u128 values
pub fn calc_earnings(taxes: Vec<u128>) -> Vec<u128> {
    taxes.iter().map(|x| jar_round(&mut x.clone(), 7)).collect()
}

/// This function is used to calculate the fees
///
/// # Arguments
///
/// * `taxes` - A vector of u128 values
///
/// # Returns
///
/// * A vector of u128 values
pub fn calc_fees(taxes: Vec<u128>) -> Vec<u128> {
    taxes.iter().map(|x| jar_round(&mut x.clone(), 3)).collect()
}

/// This function is used to calculate the jar_round value
///
/// # Arguments
///
/// * `value` - A mutable reference to a u128 value
/// * `percent` - A u128 value
///
/// # Returns
///
/// * A u128 value
pub fn jar_round(value: &mut u128, percent: u128) -> u128 {
    let div: u128 = *value * percent / 10 / 10;
    let mut remainder = *value * percent / 10 % 10;
    if remainder <= 4 {
        remainder = 0;
    } else {
        remainder = 10;
    }
    *value = div + remainder;
    *value
}

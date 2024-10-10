mod guardian;
mod validation;
use guardian::{
    read_json_to_guardians, validate_and_return_errors, write_guardians_to_file, Guardian,
};

fn main() {
    let guardians: &mut Vec<Guardian> =
        &mut read_json_to_guardians("test-data.json").unwrap_or_default();
    let output = validate_and_return_errors(guardians);
    write_guardians_to_file(output, "output.txt").expect("Couldn't write to the file.");
}

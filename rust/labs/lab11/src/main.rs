use crate::utils::{processor, read_file};
mod utils;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let inscriptions: Vec<String> = read_file("t.txt")?;

    let result = processor(inscriptions)?;

    println!("\nNEW BEARNING ELEMENT: \t {:?}", result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_files_results() {
        let files_and_results = [
            ("data.txt", 300),
            ("test2-data.txt", 282),
            ("test-data.txt", 282),
        ];

        for (file, result) in files_and_results.iter() {
            let inscriptions: Vec<String> = read_file(file).unwrap();
            let processed = processor(inscriptions).unwrap();
            assert_eq!(processed, *result);
        }
    }
}

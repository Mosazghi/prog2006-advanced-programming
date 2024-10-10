mod utils;
use crate::utils::*;
use std::time::Instant;

fn main() {
    let start = Instant::now();
    let taxes: Vec<u128> = read_from_file_parse_to_vector("txs_real.txt");
    let taxes_to_ints: Vec<u128> = taxes.iter().map(|x| x * MULTIPLIER).collect();
    // let mut jar_def: u128 = 0; // Unecessary variable ???
    // ** Do calculations ** //
    let proper_sum = taxes.iter().sum::<u128>() / 100;

    let fees: Vec<u128> = calc_fees(taxes_to_ints.clone());
    let proper_fee_sum = (taxes.iter().sum::<u128>() * 3 / 10) as f64 / 100.0;
    // jar_def = 0; // Need to reset it
    let earnings: Vec<u128> = calc_earnings(taxes_to_ints.clone());
    let proper_earnings_sum = (taxes.iter().sum::<u128>() * 7 / 10) as f64 / 100.0;

    // ** Write the results to files ** //
    write_to_file("fees.txt", fees);
    write_to_file("earnings.txt", earnings);

    // ** Read from file ** //
    let fees: Vec<u128> = read_from_file_parse_to_vector("fees.txt");
    let fees_sum = fees.iter().sum::<u128>() as f64 / 1000.0;

    let earnings: Vec<u128> = read_from_file_parse_to_vector("earnings.txt");
    let earnings_sum = earnings.iter().sum::<u128>() as f64 / 1000.0;

    // ** Get the potential error ** //
    let error_sum = proper_sum as f64 - (earnings_sum + fees_sum) as f64;
    let error_fee_sum = proper_fee_sum as f64 - fees_sum;
    let error_earnings_sum = proper_earnings_sum as f64 - earnings_sum;
    let duration = start.elapsed();
    // ** Assert the results (quick tests because i'm lazy) ** //
    assert_eq!(error_sum, 0.0);
    assert_eq!(error_fee_sum, 0.0);
    assert_eq!(error_earnings_sum, 0.0);
    // ** Print the results ** //
    print!("\nSUM:\t\t\t{:.2}", (earnings_sum + fees_sum) as f64);
    println!(" | ERROR: {:.2}", error_sum);

    print!("FEES_SUM:\t\t{:.2}", fees_sum as f64);
    println!(" | ERROR: {:.2}", error_fee_sum);

    print!("EARNINGS_SUM:\t\t{:.2}", earnings_sum as f64);
    println!(" | ERROR: {:.2}", error_earnings_sum);
    println!("Duration: {:?}", duration);
}

# Lab 06

#### multiply by 1000

        0.01* 1000 = 10 

#### find 30% of 1 cent

        10 `div` (/) 30 = 0

#### and find the remainder (that is in the jar)

        10 / 3 `mod` 10 = +3

#### after calc.ing the jar, the result for 1. trans

        0.03 ~ 0 (0.00)

#### for the second transaction

        10 `div`30 = 0, remainder = +3

#### add these two remainders together

        3+3 = 6 round up to 10 (1..=4,5..=10)

#### the result for 2nd trans

        6 ~ 10 (0.01)

#### now the defecit in the jar is

        6 - 10 = -4

#### result for 3rd

        in jar: -1 (+3)
        0  ~ 0 (0.00)

#### result for 4th

        in jar = +2
        0 ~ 0 (0.00)

#### result for 5th

        in jar: +5
        5 ~ 10 (0.01)

#### now add all of the transactions

        0.00 + 0.01 + 0.00 + 0.00 + 0.01 = 0.02 (±0.05)
         real is 0.15

```rs
use crate::utils::{read_from_file_parse_to_vector, write_to_file};

mod utils;
// use crate::utils::{calc, jar_round, read_from_file_parse_to_vector, write_to_file};
const MULTIPLIER: u128 = 1000000000;
fn main() {
    let taxes: Vec<u128> = read_from_file_parse_to_vector("txs_real.txt");
    let to_ints: Vec<u128> = taxes.iter().map(|x| x * MULTIPLIER).collect();
    let mut res: Vec<u128> = vec![0; taxes.len()];
    let mut jar_def = 0;
    for i in 0..res.len() {
        let div = to_ints[i] * 3 / 10 / 10;
        let mut remainder = to_ints[i] * 3 / 10 % 10;
        let temp = remainder;
        remainder += jar_def;
        if remainder <= 4 {
            remainder = 0;
        } else {
            remainder = 10;
        }
        jar_def = temp - remainder;
        res[i] = div + remainder;
    }
    let output: u128 = res.iter().sum::<u128>() / MULTIPLIER / 10;
    println!("output:\t\t {:?}", output);
    let proper_sum = (taxes.iter().sum::<u128>() * 3 / 10) / 100;
    println!("proper sum:\t {:.2}", proper_sum);
    println!("res:\t\t {:.2}", output);
    println!("err: \t\t {:.2}", proper_sum - output);
    write_to_file("hmm.txt", res);
}
```

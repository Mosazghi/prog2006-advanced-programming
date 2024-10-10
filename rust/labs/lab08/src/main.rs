use std::cmp::Ord;
use std::cmp::Ordering;
use std::time::Instant;

const TEST_NUMBER_1: u128 = 28;
const TEST_NUMBER_2: u128 = 496;
const TEST_NUMBER_3: u128 = 2305843008139952128;
// const TEST_NUMBER_4: u128 = 2658455991569831744654692615953842176;
// const TEST_NUMBER_5 : ??? = 191561942608236107294793378084303638130997321548169216;

fn main() {
    println!("======== Lab 08 fibonacci ========");
    println!("10th Fibonacci number:\nlazy:\t\t {}", fib(10));
    println!("loop:\t\t {}", fibonacci_l(10));
    println!("recursive:\t {}", fibonacci_r(10));
    // The largest number for n that can be put into fibonacci seq. is 93.
    test_fib();

    println!("======== Lab 08 numbers ========");
    for i in [TEST_NUMBER_1, TEST_NUMBER_2, TEST_NUMBER_3].iter() {
        let start_time = Instant::now();
        let result = classify_number(*i).unwrap();
        let duration = start_time.elapsed();
        println!(
            "The number {} is {:?}\n\tCalculated in: {} nano secs",
            *i,
            result,
            duration.as_nanos()
        );
    }
    test_numbers();
    test_aliquot();
    println!("100: {}", aliquot(100));
}

// Test function for the Fibonacci implementations.
fn test_fib() {
    // Can we test for lazy 0th element?
    assert_eq!(fib(0), 0);
    assert_eq!(fibonacci_l(0), 0);
    assert_eq!(fibonacci_r(0), 0);
    for (q, a) in [
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 3),
        (5, 5),
        (6, 8),
        (7, 13),
        (8, 21),
        (9, 34),
        (10, 55),
        (11, 89),
    ]
    .iter()
    {
        assert_eq!(fib(*q), *a);
        // println!("[TESTING] {} ==? {} ({q})", fibonacci_l(*q as u64), *a);
        assert_eq!(fibonacci_l(*q as u64), *a);
        assert_eq!(fibonacci_r(*q as u64), *a);
    }
}

fn test_numbers() {
    // assert_eq!(classify_number(0), None);

    for n in [1, 2, 3, 4, 5, 8, 9, 10, 11].iter() {
        println!("Testing: {}", *n);
        assert_eq!(classify_number(*n), Some(NumberType::Deficient));
    }

    for n in [6, 28, 496, 8128, 33550336, 8589869056, 137438691328].iter() {
        println!("Testing: {}", *n);
        assert_eq!(classify_number(*n), Some(NumberType::Perfect));
    }

    for n in [12, 18, 20].iter() {
        println!("Testing: {}", *n);
        assert_eq!(classify_number(*n), Some(NumberType::Abundant));
    }

    // harder, longer cases
    // assert_eq!(classify_number(2305843008139952128), Some(NumberType::Perfect));
}

// test for the aliquot sum
fn test_aliquot() {
    for (q, a) in [
        (1, 0),
        (2, 1),
        (3, 1),
        (4, 3),
        (5, 1),
        (6, 6),
        (7, 1),
        (8, 7),
        (9, 4),
        (10, 8),
        (11, 1),
        (12, 16),
        (13, 1),
        (14, 10),
        (15, 9),
        (16, 15),
        (17, 1),
        (18, 21),
        (19, 1),
        (20, 22),
    ]
    .iter()
    {
        println!("[Testing] {} ==? {}", *q, *a);
        assert_eq!(aliquot(*q), *a);
    }
}

// Implementation of the Fibonacci sequence with a loop.
fn fibonacci_l(n: u64) -> u64 {
    let mut a = 0;
    let mut b = 1;
    for _ in 0..n {
        let c = a + b;
        a = b;
        b = c;
    }
    a
}

// Implementation of the Fibonacci sequence with recursion.
fn fibonacci_r(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci_r(n - 1) + fibonacci_r(n - 2),
    }
}

// ========= Lab 08 numbers =========

// Implementation of the aliquot sum.
// Since the aliquot sum for n = 0 is considered undefined, we need cannot return a number to reflect this.
// Instead, one option, may be using an return of type `Option<u64>`.
fn aliquot(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 0,
        _ => {
            let mut sum = 1; // 1 is a divisor for all numbers
            let sqrt_n = (n as f64).sqrt() as u64; // we only need to check up to the square root of n by doing this method

            for i in 2..=sqrt_n {
                // we start from 2 since 1 is already added to the sum
                if n % i == 0 {
                    sum += i;
                    // for  every divisor i that is found,
                    // there will be a corresponding divisor n/i, and one of these
                    // will always be less than or equal to the square root of n.
                    let corresponding_divisor = n / i;
                    if corresponding_divisor != i {
                        sum += corresponding_divisor;
                    }
                }
            }
            print!("{}: ", sum);
            sum
        }
    }
}

// Implementation of the number type.
#[derive(Debug, PartialEq, Eq)]
enum NumberType {
    Perfect,
    Deficient,
    Abundant,
}

// Implementation of number classification.
// Why do we use u128 here? Why not u64? What is the performance impact?
fn classify_number(n: u128) -> Option<NumberType> {
    let res: u128 = aliquot(n as u64) as u128;

    match res.cmp(&n) {
        Ordering::Equal => Some(NumberType::Perfect),
        Ordering::Less => Some(NumberType::Deficient),
        Ordering::Greater => Some(NumberType::Abundant),
    }
}

// ========= Lab 08 fibonacci lazy =========

// Implementation of the Fibonacci sequence with the lazy iterator
// Please study the code below and think of answers for the following questions:
// 1. What is the purpose of the `Fibs` struct?'
// -> We are using non-recursive method here. In this case the purpose for this struct
// is to get hold of its necessary variables (next & curr) for computation.
// 2. What is the purpose of the `new` function?
// -> Initializes a new instance of `Fibs`
// 3. What is the purpose of the `Iterator` trait?
// -> It allows us to use iterators.
// 4. What is the purpose of the `next` function?
// -> It's it required as it allows us to retrieve the next value in our iterator.
// 5. What is the purpose of the `fib` function?
// -> To calculate and return the nth fib. sequence.
fn fib(n: usize) -> u64 {
    if n == 0 {
        return 0;
    }
    let fibs = Fibs::new();
    *fibs.take(n).collect::<Vec<u64>>().last().unwrap()
}

struct Fibs {
    curr: u64,
    next: u64,
}

impl Fibs {
    fn new() -> Fibs {
        Fibs { curr: 0, next: 1 }
    }
}

impl Iterator for Fibs {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let new_next = self.curr + self.next;
        self.curr = self.next;
        self.next = new_next;
        Some(self.curr)
    }
}

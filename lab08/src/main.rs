// Lab 08
use std::time::Instant;

const TEST_NUMBER_1 : u64 = 28;
const TEST_NUMBER_2 : u64 = 496;
const TEST_NUMBER_3 : u64 = 2305843008139952128;
// const TEST_NUMBER_4 : u128 = 2658455991569831744654692615953842176;
// const TEST_NUMBER_5 : ??? = 191561942608236107294793378084303638130997321548169216;


fn main() {
    println!("======== Lab 08 fibonacci ========");
    println!("10th Fibonacci number:\nlazy:\t\t {}", fib(10));
    println!("loop:\t\t {}", fibonacci_l(10));
    println!("recursive:\t {}", fibonacci_r(10));

    test_fib();

    println!("======== Lab 08 numbers ========");
    for i in [TEST_NUMBER_1, TEST_NUMBER_2, TEST_NUMBER_3].iter() {
        let start_time = Instant::now();
        let result = classify_number(*i as u128).unwrap();
        let duration = start_time.elapsed();
        println!("The number {} is {:?}\n\tCalculated in: {} nano secs", *i, result, duration.as_nanos());
    }
    test_numbers();
}


// Test function for the Fibonacci implementations.
fn test_fib() {
    // Can we test for lazy 0th element? 
    // assert_eq!(fib(0), 0);
    assert_eq!(fibonacci_l(0), 0);
    assert_eq!(fibonacci_r(0), 0);
    for (q, a) in 
        [(1, 1), (2, 1), (3, 2), (4, 3), (5, 5), 
         (6, 8), (7, 13), (8, 21), (9, 34), (10, 55), (11, 89)].iter() {
        assert_eq!(fib(*q), *a);
        assert_eq!(fibonacci_l(*q as u64), *a);
        assert_eq!(fibonacci_r(*q as u64), *a);
    }
}

fn test_numbers() {
    assert_eq!(classify_number(0), None);
    
    for n in [1, 2, 3, 4, 5, 8, 9, 10, 11].iter() {
        assert_eq!(classify_number(*n), Some(NumberType::Deficient));
    }
    
    for n in [6, 28, 496, 8128, 33550336, 8589869056, 137438691328].iter() {
        assert_eq!(classify_number(*n), Some(NumberType::Perfect));
    }
    
    for n in [12, 18, 20].iter() {
        assert_eq!(classify_number(*n), Some(NumberType::Abundant));
    }

    // harder, longer cases
    // assert_eq!(classify_number(2305843008139952128), Some(NumberType::Perfect));
}


// Implementation of the Fibonacci sequence with a loop.
fn fibonacci_l(n: u64) -> u64 {
}

// Implementation of the Fibonacci sequence with recursion.
fn fibonacci_r(n: u64) -> u64 {
}



// ========= Lab 08 numbers =========

// Implementation of the aliquot sum.
fn aliquot(n: u64) -> u64 {

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

}



// ========= Lab 08 fibonacci lazy =========



// Implementation of the Fibonacci sequence with the lazy iterator
// Please study the code below and think of answers for the following questions:
// 1. What is the purpose of the `Fibs` struct?
// 2. What is the purpose of the `new` function?
// 3. What is the purpose of the `Iterator` trait?
// 4. What is the purpose of the `next` function?
// 5. What is the purpose of the `fib` function?
fn fib(n: usize) -> u64 {
    let fibs = Fibs::new();
    *fibs.take(n)
    .collect::<Vec<u64>>()
    .last()
    .unwrap()
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

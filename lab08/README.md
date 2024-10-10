# Lab 08: numbers in Rust

* Deadline: Friday, 8th of March, 16:00
* Final deadline: Wednesday, 27th of March, 16:00
* Score: 5
* Difficulty: easy/intermediate
* Note - the challenge is to make the program run in reasonable time for TEST_NUMBER_3
* ChatGPT: **do not use it**. Write all the code yourself.
* Co-pilot: **do not use**.
* Coding: code only by yourself. Use Rust book and Rust documentation. Do not use any external crates.




# Fibonacci

* `fibonacci`
   * Implement the function `fibonacci` to return the `nth` fibonacci number from the Fibonacci sequence.
   The fibonacci sequence we assume here is: `0,1,1,2,3,5,8,..` and so on.
   * Implement it with recursion `_r`, and loop `_l` as provided by signature template in the Rust lab project.
   * What is the largest `n` we can calculate the fibonacci number for when using the `u64` data type?
* Consider the lazy implementation with `fib()` and `Fibs`, and understand how it works.



# Numbers

[Aliquot sum](https://en.wikipedia.org/wiki/Aliquot_sum) is the sum of all the proper divisors of a number.
That is, the sum of all divisors other than the number itself.
For example, the `aliquot sum` for number `4` is `3 = 1 + 2`, and for number 8 is `7 = 1 + 2 + 4`.
Note, `aliquot` for `0` is `undefined`, and for `1` is `0`

* [Perfect number](https://en.wikipedia.org/wiki/Perfect_number) is a number which is equal to its `aliquot sum`.
* [Deficient number](https://en.wikipedia.org/wiki/Deficient_number) is a number for which `aliquot sum` is less than the number itself.
* [Abundant number](https://en.wikipedia.org/wiki/Abundant_number) is a number for which `aliquot sum` is more than the number itself.



## Tasks

* `aliquot`
   * Implement the function `aliquot` that given a number, returns its aliquot.
   * Why is the signture returning Option<u64> instead of `u64`?
* `classify_number`
   * Implement the function `classify_number` that is provided in the project.
   * Why is the signature returning `Option<NumberClassification>` instead of `NumberClassification`?
* Check, if `6`, `28`, `496` and `2305843008139952128` are **perfect numbers**.
* Can you also check if TEST_NUMBER_4 `340282366920938463463374607431768211455` is a perfect number? Why not?



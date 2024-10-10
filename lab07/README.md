# Lab 07: Intro to Rust

* Deadline: Friday, 1st of March, 16:00
* Final deadline: Wednesday, 27th of March, 16:00
* Score: 4
* Difficulty: easy
* ChatGPT: **do not use**. Write all the code yourself.
* Co-pilot: **do not use**.
* Coding: code only by yourself. Use Rust book and Rust documentation. Do not use any external crates.



# Important

* Pay attention to the coding style.
* Make the code easy to maintain, readable, and concise. 
* Pay attention to the code performance. Use "single pass" as much as possible.



# Acronym generator

* `acronym`
   * Implement the function `acronym` to return the capitalised acronym of a given text sequence.
   * Capital letters should stay capitalised, however, if there is an acronym in the phrase, only the first letter should be used.
   * First letter of words should be used, and capitalised.
   * Whitespace characters, tabs, underscore, vertical bar | and dash _ (`_ -\t\n|`) should be treated as word separators.
   * Check the code for examples and test cases.



# Reverse a string

* `reverse`
   * Implement the function `reverse` to return the reversed input string.
   * **DO NOT USE** `rev()` built-in function for this task.



# Birds watching

The natural scientist, Sarah, has been observing birds, and noting their sightings in a log journal.
She has given you the data in a form of list of bird IDs, as strings.  Your task is to create two functions.
One, `find_most_frequent_bird` and `find_most_frequent_bird_count`.
Note, that the most frequent bird might be more than a single bird, eg. two different birds, AA and BB, might be observed the same number of times, and both might be the most frequent. Which one should be returned?  To solve this, Sarah decided to actually create two different implementions, one which always prints the most frequent bird that has been observed FIRST (`find_most_frequent_bird`), and one, in which one of the most frequent birds is returned, but, the order does not matter (`find_most_frequent_bird_no_order`).
* `find_most_frequent_bird_count`
   * This functions, goes over the log data, and checks which birds 
     were the most frequently sighted and reports the total count of the sightings.
* `find_most_frequent_bird_no_order`
   * This function, goes over the log data and checks which birds were the most sighted.
   * Then, it reports the most sighted bird ID string.
   * If there are multiple birds that have been sighted the same number of times, 
   the reported bird is picked "at random".
* `find_most_frequent_bird`
   * Same as above, but this time, in case of a tie, in case there are more than a single bird with the same number of sightings,
   the bird that was observed first must be returned. 

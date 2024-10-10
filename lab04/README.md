# Lab 04

* Deadline: Friday, 9th of February, 16:00
* Final deadline: Friday, 1st of March, 16:00
* Score: 5
* Difficulty: medium
* ChatGPT: **use only** for explanations. Do not use to generate code.
* Co-pilot: **do not use**
* Coding: code it all yourself. **ONLY** use code that you understand.
* Resources:
   * Haskell books
   * GHCi (interactive interpreter)
   * hoogle.haskell.org


# Objectives

* Basics of error handling with `Maybe` and `Either` types.
* You should try **not** use ChatGPT or co-pilot for code generation. 
* If you do, make it generate code that you want to generate. Code that you understand.
* **Do not copy-paste code** from ChatGPT without READING it and understanding it.




# The task: Galactic Greetings

**Breaking News: Galactic Greetings! Earth Receives Signals from Extraterrestrial Neighbors!**

Hey Earthlings! Hold on to your telescopes because we've got some cosmic news. Our blue planet has just received a cosmic "Hello World" from an advanced alien civilization! 🛸👽

To make sense of these mind-boggling transmissions, Earth's brightest minds have set up a cutting-edge research lab. Picture lab coats, blinking lights, and scientists peering into strange-looking screens — yes, just like in the movies!

Our lab experts are decoding daily transmissions of mind-blowing photonic signals. These signals arrive in the form of a sequence of positive integers. But, there's a cosmic catch! Communicating with our alien pals is only possible if the communication meets certain conditions, and there is no cosmic interference! 

**Transmission Decoding Manual:**

1. **Unique Min-Max Club:** To start the interstellar chat, the minimum and maximum numbers in the transmission sequence must be like unicorns — **unique**! If they're not, well, it's like trying to have a phone call with bad reception. No-go!

2. **Even Sum Extravaganza:** The sum of the minimum and maximum numbers must be an even number — a real party animal divisible by 2. If it's odd, it's like trying to dance the tango alone. Awkward!

3. **Counting Cosmic Vibes:** Now, here's the fun part. Count the number of times the minimum plus maximum, divided by 2, appears in the transmission. That magical number is our extraterrestrial message.

**Your Mission, Should You Choose to Accept:**

You, the stellar programmer, have been chosen to implement a software system for decoding the messages.
This system should take the alien transmission sequence as input and return the coded message — the cosmic number! 🚀

**For Example:**

For the transmission:
```
5 5 5 8 1 2 3 4 9 8 2 3 4
```
No cosmic interference detected!
- Minimum number: 1 (unique!)
- Maximum number: 9 (also unique!)
- Sum: 10 (even and divisible by 2)
- Min + Max divided by 2: 5 (magic number!)
- Occurrences of 5: 3 (our cosmic message!)
- **The message is 3! 🌌**

But beware, for transmissions like:
```
5 5 5 8 1 2 3 4 9 8 2 3 4 1
```
Oh no! There's cosmic interference because the minimum number isn't unique. Communication hiccup! 📡🤷‍♀️

So, get ready for an interstellar coding adventure, and let's decipher the secrets of the cosmos together! 🌌👾
 

# Instructions

* Make your code modular. Split the functionality into individual functions
that are composed together to form the final result
* Keep all your functions pure, and only use `getContents` in the `main` functions
* Use `doctests` to test your pure functions before you even implement them! It helps with debugging!
* Make sure that ALL your implemented functions have at least few test cases as `doctests`
* Make sure that `stack test` executes all your doctests and they are all OK


## Gitlab and git

The `lab04` project is pre-initiated for the task. Fork the project into your own workspace
and populate the missing parts.  Make sure that the test harness from the lab04 project 
passes correctly. You will find useful test cases already there. 


# Bonus (+1)

Rewrite the `decodeMessage` function such that it provides meaningful error messages when
communication interference is detected. 



# IMPORTANT

Generated code is discouraged, and writing the solution by hand is encouraged.
Only use code you understand. Do not use operators that you do not understand.
* Do not use `do-notation` unless you understand Monads and can-rewrite your code with the manual bind operator `(>>=)`
* Do not use applicatives unless you understand applicatives and `<$>` and `<*>` operators. 

We will cover `Applicatives` and `Monads` later in the course. But, if you already have understanding how to use them, then you can use them! Note however, you should be able to rewrite your own code without the do-notation when asked. 


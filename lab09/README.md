# Lab 09: Atlantis

* Deadline: Friday, 15th of March, 16:00
* Final deadline: Wednesday, 27th of March, 16:00
* Score: 8 for Haskell, 6 for Rust.
* Difficulty: intermediate
* Language:
   * Haskell - preferably. Haskell provides the most concise and clear solution.
   * Rust - the language will limit your expressive abilities.
* ChatGPT: **use it**. Try to make it write the code for you.
* Co-pilot: **use it**. Be critical, and only select code that elegantly solves the problem.
* Coding: design the solution by yourself. The solution must be:
   * elegant
   * composable
   * easy to update and modify
   * easy to maintain
   * obvious
   * avoid using `if`, `match`, and `case` expressions.
   * if you can, make use of Applicative functors.




# The Lost Treasure of Atlantis

In the mystical land of Atlantis, a legendary treasure lies hidden within the ruins of an ancient temple. To unlock the temple's secrets and claim the treasure, one must possess the sacred artifact known as the "Key of Elements."

As the Atlantean explorer, you embark on a quest to find the lost treasure. However, your journey is filled with perilous challenges, including a magical gateway that guards the temple entrance.

To pass through the gateway, you must present a valid Key of Elements, which is created by combining four elemental gems: Earth, Water, Fire, and Air. Each gem is hidden in treacherous locations guarded by mythical creatures and ancient traps.
If you present an invalid Key of Elements, you will pay the highest price - your life.

With the fate of Atlantis hanging in the balance, you must gather the elemental gems, forge the Key of Elements, and unlock the secrets of the lost temple.
Only by mastering the art of validation and navigating the treacherous paths of Atlantis can you uncover the truth behind the lost treasure and fulfill your destiny.


# Key of Elements

Here are four validation tasks, each representing one of the elemental gems: Earth, Water, Fire, and Air:

1. **Earth Validation**: Validate a numerical input representing the weight of a precious greenstone. The validation criteria include ensuring that the weight is within 13-113 grams, and it is an even number. The relevant error messages are:
* Greenstone weight: not a number
* Greenstone weight: outside the range
* Greenstone weight: not even

2. **Water Validation Task**: Validate a text input representing the name of a mythical creature guarding the Water gem. The validation criteria include checking that the name contains exactly two lower-case 'a' characters, and that it starts with capital letter, and is within 3 and 15 characters long (inclusive). The relevant error messages are:
* Water guardian name: outside the range
* Water guardian name: not capitalised
* Water guardian name: missing 'a'

3. **Fire Validation Task**: Validate a numerical input representing the temperature of a volcanic cave where the Fire gem is located. The validation criteria include ensuring that the temperature is within 400-700 degrees Celcius, and that it is an even number. Relevant error messages are:
* Vulcanic cave temperature: outside the range
* Vulcanic cave temperature: not even
* Vulcanic cave temperature: not a number

4. **Air Validation Task**: Validate a text input representing a riddle inscribed on a mysterious stone tablet in the sky kingdom where the Air gem is hidden.
The validation criteria include checking if the riddle starts with a captial letter, and if it ends with a dot.
The riddle should contain even number of lower-case vowels: "aouei". Relevant error messages are:
* Sky riddle: not capitalised
* Sky riddle: missing dot
* Sky riddle: not even


# Data

The data is provided in forms of an array of JSON objects representing the four elements collected during the journeys. The data should be checked, and the output should be a text file, with rows starting with the Key of Elements id, followed by colon (:), followed by coma-separated list of validation errors, or word "Valid" to highlight the valid Key of Elements entries.

# Hints


* JSON parsing is required for this lab. Use `aeson` dependency (Haskell) or `serde`, and `serde-json` (Rust), with deriving the parsers automatically, preferably (less manual work, less chances of errors). Use ChatGPT and co-pilot to help you with it.
* "Not A Number" error can be handled manually by extracting all the fields as they are, as JSON Value, and then, making manual processing. But this is **discouraged**. What is encouraged is to dedicated parsing to JSON library, and let it enforce the type for us.
In this scenario "Not A Number" is a special case handled by the parsing itself, BEFORE we actually validate the data.
* `Maybe`, (or `Option`) is not suitable, because on the unhappy path it does not carry any errors, just gives us `Nothing`
* `Either` is also not suitable, because it stops on the first error and only returns one error. We want
  something similar to `Either`, or `Result`, but something that allows us to collect/combine multiple errors together.
* Think what structure would allow you to combine processing and achieve the desired
  composition of validations and collecting all the errors.
  Use ChatGPT/co-pilot and Internet search to help you with ideas.
* Both, for Haskell andRust, have a look at `Data.Validation`. It is not allowed to use it as dependency (in Haskell), but it is allowed to
  define it for yourself and use your own `Validation` type.
* Repeated code blocks, repeated expressions or names, aka violations of DRY, are not acceptable.
* Magic numbers and strings in the code should be avoided. Use constants instead.
* Try to make the code re-usable and modular as much as you can.
* Make it elegant.

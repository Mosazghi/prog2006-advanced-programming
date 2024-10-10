# Lab 11: Bearing in Space

* Deadline: Friday, 5th of April, 16:00
* Final deadline: Wednesday, 12th of April, 16:00
* Score: 4
* Difficulty: easy
* ChatGPT: **decide for yourself**
* Co-pilot: **decide for yourself**
* Language: Rust




# Space Voyager

In the not-so-distant future, humanity has made remarkable strides in space exploration.
A team of intrepid astronauts embarks on a groundbreaking mission to explore a distant
exoplanet located in a distant galaxy.
Their spacecraft, the *BPROG Voyager*, is equipped with state-of-the-art technology and
manned by a crew of seasoned explorers and problem solving programmers.

During their journey through the cosmos, the crew encounters an anomaly —- 
an ancient alien artifact floating in the void of space.
Intrigued by the artifact's origin and purpose, they decide to investigate further.

Upon closer inspection, the artifact reveals inscriptions etched into its surface, written
in an enigmatic alien language.
The crew suspected that deciphering these inscriptions may hold the key to unlocking
the secrets of the artifact and understanding the advanced technology of the alien civilization.

As the crew deciphers the inscriptions, they realize that the symbols represent
alpha-numerical values associated with spatial coordinates and celestial navigation.
The astronauts hypothesize that these numbers could provide crucial information
for plotting their course through space and determining the required direction of travel.
To do that, they need to calculate the new azymuth (bearing) for their ship.

The inscriptions are provided as lines of alpha-numerical sequences in the `data.txt` file.


## The task

Your task is to develop a program to analyze the inscriptions on the alien artifact,
extract the numerical values encoded within, and use them to calculate the new component of the azymuth of the *BPROG Voyager*'s travel.


1. **Decipher the Inscriptions**: Write a program to read the alien inscriptions from the **standard input**. Each line of input provides a single inscription.
2. **Extract 2-digit numbers**: Each inscription consists of a series of alpha-numerical symbols. The first and the last decimal digit represent the left and right parts of the two-digit numerical value in decimal.
3. **Digits as english words**: Some of the digits are actually spelled out with letters: `one, two, three, four, five, six, seven, eight`, and `nine` also count as valid "digits".

For example:
```
two3nine
eightwofour
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
```

In this example, the extracted 2-digit values are `29, 84, 13, 24, 42, 14, 76`.
Adding these together produces `282`.
Calculating `282 mod 360` gives us `282` which is the new bearing component that we need to report back.

What is the new bearing component of the *BPROG Voyager* based on the inscription data?



# Lab 06

* Deadline: Friday, 23rd of February, 16:00
* Final deadline: Friday, 27th of March, 16:00
* Score: 5
* Difficulty: medium-hard
* ChatGPT: **use** for explanations and to generate code. Use to get ideas.
* Co-pilot: **use to generate functions**
* Coding: code as much with AI-tools as you want. **ONLY** use code that you understand.
* Langague: **Haskell** or **Rust**
   

# Objectives

* Advanced number manipulation
* Currency management
* If you do, make it generate code that you want to generate. Code that you understand.
* **Do not copy-paste code** from ChatGPT without READING it and understanding it.




# The task: "The Currency Whisperer: Unveiling the Godfather's Finances"

In the dimly lit back alleys of New York City, whispers of transactions echo through the corridors of power. Don Corleone, the enigmatic figurehead of the underworld, commands respect and fear in equal measure. Yet, even the Don cannot escape the grasp of suspicion when it comes to his financial dealings. Rumors swirl that his accountant, Guido, may be cooking the books, skimming off the top of the Godfather's empire for his own gain. Desperate to maintain control and integrity within his organization, the Don turns to an unlikely ally, **YOU**, a master programmer known only as "The Currency Whisperer."

You, esteemed programmer, have been summoned to the lavish chambers of the Corleone estate. Don Corleone himself entrusts you with a task of utmost importance: to scrutinize the labyrinthine ledgers and ensure that Guido's calculations are as straight as the barrel of a Tommy gun. Your mission is twofold:

1. **Audit the Books:**
   - You will be provided with a file containing one million numbers, each representing transactions within the Corleone syndicate.
   - The transactions are in an unkown currency. However, the currency looks and behaves like United States dollars - it has the dollar part, and two decimal places for the cents. 1 cent is the smallest undivisible unit, and no transactions can occur with sub-cent precision. The smallest amount that can be paid, or received as payment, is 1 cent. 
   - Your task is to wield your programming prowess to generate two additional files:
     - One file will detail the 30% "protection fees" extracted from each transaction, akin to the tithe paid to the Don.
     - The other file will reveal the remaining 70%, representing the true earnings of the organization.
   - But beware – any discrepancy could spell disaster to yourself!

2. **Total Sum Analysis:**
   - Armed with your algorithms, calculate the total sum of:
     - All transactions.
     - All fees collected.
     - All earnings accrued.
   - Compare these sums against the 30% extracted from the total transactions. The Don demands absolute precision – any deviation must be explained.

As you delve into the digital underworld, remember: in this world of shadows and secrets, only the code speaks truth. The fate of the Corleone empire rests in your hands. Will you be the silent guardian of justice or succumb to the allure of betrayal? Choose wisely, for in the game of codes, there are no second chances.


# Elaborations

* Each line represents a single transaction, with two-decimal places.
* Example data file:
```
13.96
21.13
1.03
11.87
56.23
```
* The actual transactions data file called `txs.txt`. 
* Read the `txs.txt` file, and for each row, calculates the 30% fee, and puts them into a `fees.txt` file, one fee per line.
* All numbers MUST have only two decimal places. Use proper currency notation, such that you print "1.00" for example, not "1" to represent a fee equivalent of 1.00 USD.
* Analogously to fees, calculate EARNINGS for each of the transactions. The earnings are 70%, and the dumped file should be called `earnings.txt`
* Your program should output the following data after processing the input transactions:
    * `TXS_SUM: `, which is the sum of all the numbers from  `txs.txt` file. 
       * For data above, the program should print: `TXS_SUM: 104.22`
    * `FEES_SUM:`, which is the sum of all the fees from `fees.txt` file
    * `FEES_TOTAL:`, which is the 30% calculated on `TXS_SUM`
    * `EARNINGS_SUM:`, which is the sum of all the entries in `earnings.txt` file
    * `EARNINGS_TOTAL`, which is the 70% calculated on `TXS_SUM`
    * `EARNINGS_SUM + FEES_SUM:` which is the sum of `EARNINGS_SUM` and `FEES_SUM`
* Check:
    * Is `TXS_SUM` different from `EARNINGS_SUM + FEES_SUM`? Explain why? How much different?
    * Is `FEES_SUM` different from `FEES_TOTAL`? Explain why? How much different?
    * Is `EARNINGS_SUM` different from `EARNINGS_TOTAL`? Explain why? How much different?
* **Important**
    * To pass the lab, any difference in the checks above must be equal or smaller than 1 cent.



# Video explanations

The solution and proper way of handling the lab is explained in the [video](https://youtu.be/dbLt1k90OXg).



# Trivia

Did you know - that Luca Brasi, the nervously massive gun-for-hire who was loyal to the Corleone family character in the Godfather movie was actually played by a real Colombo family mobster? Lenny Montana. Brasi first appears in the opening moments of The Godfather, stumbling over his lines as if he weren't an actual actor because he was not an actual actor. Montana was a real-life mob enforcer for the Colombo family who was on the set of the movie to make sure that the word "mafia" was never said in front of the camera.
Source: https://www.thevintagenews.com/2018/04/09/luca-brasi/

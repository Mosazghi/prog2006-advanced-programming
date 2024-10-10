# Lab 05: Terning

* Deadline: Friday, 16th of February, 16:00
* Final deadline: Friday, 1st of March, 16:00
* Score: 5
* Difficulty: intermediate hard
* ChatGPT: **use it**. Use it for explanations. You can use it to generate/modify your code. You can use it to explain code to you. 
* Co-pilot: **use is encouraged**. Use it to generate code for you!
* Coding: code with tools and by yourself. **ONLY** use code that you understand.


# Objectives

* Basic structure of more complex programs. Divide and conquer skills. 
* Ability to deal with larger problems and splitting them.
* Problems decomposition into individual parts that need to be combined together.
* You **should try to use** ChatGPT or co-pilot for code generation, and code understanding.
* Make the tools to generate code that you want and that you understand.
* Make sure you understand all the generated code. Ask ChatGPT or Co-pilot to explain the code to you.



# The task: Terning

Alright, buckle up for the ancient game of dice fun ride! Here's the lowdown on the rules:

Step 1: Roll the 4 Dice Thrice!
Grab those four dice and roll 'em like a dice maestro! Do it three times and add up the numbers each time. These numbers create the VIP club called "winning numbers." Let's say you rolled 1-1-1-3 (totaling 5), 3-2-5-1 (summing up to 11), and 4-5-2-1 (clocking in at 12). Boom! The winners are 5, 11, and 12.

Step 2: Go for the Points!
Now, roll those four dice five more times, aiming to hit the jackpot with one of the winning numbers. If you succeed, you're in for some points!

- Single digits winning numbers get you 1 point (those are: 4,5,6,7,8,9)
- Numbers between 10 and 19 snag you 2 points.
- Big shots from 20 to 24 rack up 4 points.

But here's the twist! If you roll a number that repeats, you're in for a double treat. For instance, if you nail an 11, that's 2 points (thanks to being in the 10-19 range). Roll another 11, and voila! It's now extra 4 points, so for two 11s you get 6 points in total!

Step 3: Double Trouble!
There is a SECOND TWIST! If you get a winning number repeat in the initial 3 rolls, your points double up instantly, and if the winning number occurs 3 times in the list of winning numbers, you QUADRUPLE the original scoring! So, if your winning numbers are, say, 11, 11, and 12, and you roll an 11, you're cashing in 4 points, not just 2! And if it happens that the winning numbers are 11, 11, 11 (three times 11), your first 11 will give you immediately 8 points! And if you happen to roll 11 again, thats 16 points extra, which means two 11 rolls with 3x 11 as winning numbers will give you 24 points.

Step 4: Tally Up the Fun!
All your throws are represented in a cool sequence of integer numbers. For instance, 8 14 16, representing the "winning numbers" followed by the scoring throws of: 5 8 18 16 12. Check it out and see how many points you've scored. 
For the sequence: `8 14 16 5 8 18 16 12` we have:
* one scoring 8 matching a winning 8 - 1 point (single digit)
* one scoring 16 matching a winning 16 - 2 points (numbers between 10-19)
* Total: 3 points
For the sequence `8 14 16 5 8 14 16 14`, you're rolling in with a whopping 9 points! That's 2 for the first 14, 4 for the second 14, and an extra 2 for the 16, plus 1 for the 8! Total 9 points.

Now, you're on a mission to process all the game data. Each row has the first 3 numbers as the winning numbers and the next 5 as the scoring numbers. Your task is to score each row, add 'em up all together. Let the good times roll! 

May the dice **be ever in your favor!** 🎲🔥




# Instructions

* Make your code modular. Split the functionality into individual functions
that are composed together to form the final result.
* Keep all your functions pure, and only use `getContents` in the `main` function
* Use `doctests` to test your functions
* Make sure that ALL your implemented functions have at least few test cases as `doctests`
* Make sure that `stack test` executes all your doctests and they are all OK
* Test data as comments and real data are provided in the repo: `data.txt` 
* The total for test data is: **?** 
   * First student to post the total for `data.txt` will get kudos!



# Gitlab and git

The `lab05` project is pre-initiated for the task. Fork the project into your own workspace
and populate the missing parts.  Make sure that the test harness passes correctly. 




# IMPORTANT

Generated code is encouraged. Use co-pilot and ChatGPT.
Only use code you understand. Do not use operators that you do not understand.
* Do not use `do-notation` unless you understand Monads and can-rewrite your code with the manual bind operator `(>>=)`
* Do not use applicatives unless you understand applicatives and `<$>` and `<*>` operators. 

We will cover `Applicatives` and `Monads` later in the course. But, if you already have understanding how to use them, then you can use them. You should be able to rewrite your own code without the do-notation when asked. 


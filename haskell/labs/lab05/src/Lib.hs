module Lib
  ( countScore,
  )
where

import Data.List (nub)

-- | Count the total score of the four dice game data.
-- This function is already implemented for you.
countScore :: String -> Int
countScore txt = sum $ map processLine (lines txt)

-- | Process a single line of the input data.
--
-- Three winning numbers (three first in the sequence).
-- --
-- Single digits winning numbers get you 1 point (those are: 4,5,6,7,8,9)
-- Numbers between 10 and 19 snag you 2 points.
-- Big shots from 20 to 24 rack up 4 points.
-- --
-- If a roll repeats in the 5 last rolls: double the points by it's original points ^
-- --
-- (first three) if winning number repeats, you get the following points as initials:
--        - 2x: 2 * original points
--        - 3x: 4 * original points
-- | Process a single line of the input data.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
--
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
processLine :: String -> Int
processLine line =
  let toList = map read (words line) :: [Int]
      threeWinningNumbers = winningNumbersPoints $ take 3 toList
      lastFiveNumbers = countFstOccurences $ drop 3 toList
      putStrLn $ "WAS WAS WAS WAS WAS"
      totalPoints =
        sum
          [ getSndOfFstTuple f threeWinningNumbers * sum [2 ^ i | i <- [0 .. s - 1]]
            | (f, s) <- lastFiveNumbers,
              f `elem` map fst threeWinningNumbers
          ]
   in totalPoints

-- Define the type signature of the function
winningNumbersPoints :: [Int] -> [(Int, Int)]
winningNumbersPoints numbers = nub $ map calculatePointsForNumber numbers
  where
    -- Function to calculate points for a single winning number
    calculatePointsForNumber n = (n, initialPoints n * repetitionMultiplier n)

    -- Function to determine the initial points based on the number
    -- Single digits: 1 point
    -- 10-19: 2 points
    -- 20-24: 4 points
    initialPoints n
      | n `elem` [4 .. 9] = 1
      | n `elem` [10 .. 19] = 2
      | n `elem` [20 .. 24] = 4
      | otherwise = 0

    -- Function to determine the multiplier based on the repetition of
    -- the first three numbers (i.e. the winning numbers)
    -- 2x: 2 * original points
    -- 3x: 4 * original points
    repetitionMultiplier n = case countOccurrences n of
      2 -> 2
      3 -> 4
      _ -> 1

    -- Function to count the occurrences of a number in the list
    countOccurrences x = length $ filter (== x) numbers

-- | Count the occurrences of the first element of a tuple in a list
--   and return a list of tuples with the first element and the count of occurrences
--
-- >>> countFstOccurences [4, 21, 10, 5, 4, 21, 13, 11]
-- [(4,2),(21,2),(10,1),(5,1),(13,1),(11,1)]
-- >>> countFstOccurences [4, 21, 10, 5, 4, 21, 13, 11, 10]
-- [(4,2),(21,2),(10,2),(5,1),(13,1),(11,1)]
countFstOccurences :: (Eq a) => [a] -> [(a, Int)]
countFstOccurences tuple = nub $ map (\x -> (x, countOccurrences x)) tuple
  where
    countOccurrences x = length $ filter (== x) tuple

-- | Get the second element of the FIRST a tuple with a given first element
-- >>> getSndOfFstTuple 4 [(8, 8), (4, 2), (6, 1), (7, 1), (8, 1), (9, 1), (10, 2), (21, 1), (13, 1), (11, 1)]
-- WAS WAS WAS WAS WAS 4
-- WAS WAS NOW 4
-- WAS NOW 8
-- NOW 2

-- >>> getSndOfFstTuple 10 [(4, 4), (4, 2), (6, 1), (7, 1), (8, 1), (9, 1), (10, 2), (21, 1), (13, 1), (11, 1)]
-- 2
getSndOfFstTuple :: (Eq a) => a -> [(a, b)] -> b
getSndOfFstTuple _ [] = error "Empty list"
getSndOfFstTuple n tuple = snd $ head $ filter (\(f, _) -> f == n) tuple

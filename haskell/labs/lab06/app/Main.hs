{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Lib (calc, parseToList, writeToFile)
import Text.Printf

main :: IO ()
main =
  do
    contents <- getContents
    let numbersList = parseToList contents
    let feesList = calc 0.3 numbersList
    let earningList = calc 0.7 numbersList

    writeToFile "fees.txt" feesList
    writeToFile "earnings.txt" earningList
    putStrLn "\n\tFiles fees.txt and earnings.txt have been written.\n"

    putStrLn "\n\tReadning from file... Results:\n"
    
    fees <- readFile "fees.txt"
    earnings <- readFile "earnings.txt"
    fees <- return $ parseToList fees
    earnings <- return $ parseToList earnings

    printf "TAX_SUM: %.2f\n" $ sum numbersList
    printf "FEES_SUM: %.2f\n" $ sum fees
    printf "FEES_TOTAL: %.2f\n" (0.30 * sum numbersList)
    printf "EARNINGS_SUM: %.2f\n" $ sum earnings
    printf "EARNINGS_TOTAL: %.2f\n" (0.70 * sum numbersList)
    printf "EARNINGS_SUM + FEES_SUM: %.2f\n" (sum earnings + sum fees)
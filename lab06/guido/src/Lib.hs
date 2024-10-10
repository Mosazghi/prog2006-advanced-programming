module Lib
    ( feeCalculate
    , earningsCalculate
    , convert2String
    , round2s
    , round2
    , correctSum
    , Money
    ) where

import Text.Printf (printf)

type Money = Double

-- | Calculates 30% fee on each of the transactions.
-- The fee is rounded to 2 decimal places.
feeCalculate :: [Money] -> [Money]
feeCalculate txs = map (round2 . (* (0.3 :: Money))) txs :: [Money]

-- | Calculates 70% of the earnings from the transactions.
earningsCalculate :: [Money] -> [Money]
earningsCalculate txs = map (round2 . (* (0.7 :: Money))) txs :: [Money]


-- | Converts Floats to Strings and joins them with a newline.
-- The numbers are rounded to 2 decimal places.
convert2String :: [Money] -> String
convert2String = unlines . map round2s

-- | Rounds a number to 2 decimal places. It always prints two decimal places,
-- if it is a round number, like 1, it should finish 1.00
round2s :: Money -> String
round2s = printf "%.2f"

-- | Rounds a floating point number to 2 decimal places.
round2 :: Money -> Money
round2 x = fromIntegral (round x * 100) / 100


-- ===================================

-- | Correct implementation of the sum function that converts text of
-- numbers with two decimal places into Integer values that are subsequently
-- added without any floating point error accumulation.
correctSum :: [String] -> String
correctSum txs = addDot (sum t)
    where t = map ((read . removeDot) :: String -> Integer) txs


-- | Removes the dot from the string, so that we can convert it to Integer.
removeDot :: String -> String
removeDot = filter (/= '.')

-- | Reintroduces the dot for the last two decimal places into an Integer.
addDot :: Integer -> String
addDot x = show (x `div` 100) ++ "." ++ show (x `mod` 100)


module Main (main) where

import Lib



realTxsSum, realFees, realEarnings :: Money
-- Values for `txs.txt`
-- realTxsSum =   5001000
-- realFees =     1500300
-- realEarnings = 3500700

-- Values for `txs_real.txt`
realTxsSum =   49967418751297640
realFees =     14990225625389292
realEarnings = 34977193125908348

-- Values for txs_100.txt
-- realTxsSum =   1.00
-- realFees =     0.30
-- realEarnings = 0.70

-- Values for txs_1m.txt
-- realTxsSum =  1000000010000
-- realFees =     300000003000
-- realEarnings = 700000007000

main :: IO ()
main = do
    input <- getContents
    let txsText = lines input
    -- we are reading the numbers into Money type, and keeping them as Money
    let numbers = (map read txsText :: [Money]) :: [Money]
    let txsTotal = sum numbers :: Money
    let properSum = correctSum txsText
    -- fees are rounded to 2 decimal places
    let fees = feeCalculate numbers
    let feesTotal = txsTotal * 0.3 :: Money
    -- earnings are rounded to 2 decimal places
    let earnings = earningsCalculate numbers
    let earningsTotal = txsTotal * 0.7
    writeFile "fees.txt" (convert2String fees)
    writeFile "earnings.txt" (convert2String earnings)

    putStrLn ""
    putStrLn "Guido's report:"
    putStrLn $ "Proper sum:\t" ++ properSum
    putStrLn ""
    putStrLn $ "TXS_SUM:\t" ++ round2s txsTotal ++ "\t| Money error: " ++ round2s (realTxsSum - txsTotal)
    putStrLn $ "FEES_SUM:\t" ++ (round2s . sum $ fees) ++ "\t| Money error: " ++ round2s (realFees - sum fees)
    putStrLn $ "FEES_TOTAL:\t" ++ round2s feesTotal ++ "\t| Money error: " ++ round2s (realFees - feesTotal)
    putStrLn $ "EARNINGS_SUM:\t" ++ (round2s . sum $ earnings) ++ "\t| Money error: " ++ round2s (realEarnings - sum earnings)
    putStrLn $ "EARNINGS_TOTAL:\t" ++ round2s earningsTotal ++ "\t| Money error: " ++ round2s (realEarnings - earningsTotal)



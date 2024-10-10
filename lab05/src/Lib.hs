module Lib
    ( countScore
    ) where

-- | Count the total score of the four dice game data.
-- This function is already implemented for you.
-- 
countScore :: String -> Int
countScore txt = sum $ map processLine (lines txt)


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
processLine line = undefined 

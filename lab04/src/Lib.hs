module Lib
    ( decodeMessage,
    isUnique,
    countNum,
    decodeMessageImproved
    ) where


-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Nothing

--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 8" 
-- Just 3

decodeMessage :: String -> Maybe Int
decodeMessage msg =
    let toList = map read (words msg) :: [Int]
        minNum = minimum toList
        maxNum = maximum toList
        minCheck = isUnique minNum toList
        maxCheck = isUnique maxNum toList
        sumCheck = even (minNum + maxNum)
        magicNum =  (minNum + maxNum) `div` 2
        countMagicNum = countNum magicNum toList
        res = if minCheck && maxCheck && sumCheck then Just countMagicNum else Nothing
    in res


-- | isUnique find if a given input is unique in a list 
-- >>> isUnique 1 "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- No instance for `Num Char' arising from the literal `1'
-- In the first argument of `isUnique', namely `1'
-- In the expression: isUnique 1 "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- In an equation for `it_aOeU':
--     it_aOeU = isUnique 1 "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- isUnique :: Int -> Bool  
isUnique x xs =
    let
        check = foldl (\acc e -> if e == x then acc + 1 else acc) 0 xs
    in check == 1

countNum n = foldl (\acc e -> if e == n then acc + 1 else acc) 0
-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
-- This is an improved version of the previous function, with a more
-- informative error messages.
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- WAS WAS Left "Communication interference detected: minimum number not Unique"
-- NOW Left "Communication interference detected: minimum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- WAS WAS Left "Communication interference detected: maximum number not Unique"
-- NOW Left "Communication interference detected: maximum number not Unique"
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- WAS WAS Right 3
-- NOW Right 3
--
-- >>> decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- NOW Left "Communication interference detected: midPoint not even"
decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg =
    let toList = map read (words msg) :: [Int]
        minNum = minimum toList
        maxNum = maximum toList
        minCheck = isUnique minNum toList
        maxCheck = isUnique maxNum toList
        sumCheck = even (minNum + maxNum)
        magicNum =  (minNum + maxNum) `div` 2
        countMagicNum = countNum magicNum toList
        errMsg = "Communication interference detected: "
        res
          | not minCheck = Left $ errMsg ++ "minimum number not Unique"
          | not maxCheck = Left $ errMsg ++ "maximum number not Unique"
          | not sumCheck = Left $ errMsg ++ "midPoint not even"
          | otherwise = Right countMagicNum
    in res

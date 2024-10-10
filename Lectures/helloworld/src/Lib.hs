module Lib
    ( someFunc
    , printHello
    , printHelloName
    , swap'
    , sum'
    , swap3
    , swap3old
    , think
    , toRoman
    , replace
    , int3string
    ) where

-- | swap first and last element of 3-num list
--
-- >>> swap3 [1,2,3]
-- [3,2,1]
--
-- >>> swap3 []
-- []
--
-- >>> swap3 [1]
-- []
--
-- >>> swap3 [1,2,3,4]
-- []
swap3 :: [a] -> [a]
swap3 l
  | length l /= 3 = []
  | otherwise = [l !! 2, l !! 1, l !! 0]


-- | Originally generated function
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Prints Hello to the terminal
printHello :: IO ()
printHello = putStrLn "Hello"

-- | Prints Hello name, where name comes as an argument.
-- Note the point free notation.
printHelloName :: String -> IO()
printHelloName = putStrLn . ("Hello " ++)

-- | Swaps tuples.
--
-- Examples:
-- >>> swap' (1,4)
-- (4,1)
--
-- >>> swap' ("Hello", "World")
-- ("World","Hello")
--
swap' :: (a, b) -> (b, a)
swap' (a, b) = (b, a)
-- swap' t = (snd t, fst t)

-- | Adding numbers from a list together.
--
-- >>> sum' [0]
-- 0
--
-- >>> sum' [1, 2, 2]
-- 5
sum' :: Num a => [a] -> a
sum' = sum



-- | Think of a number game....
-- think :: (Integral a) => a -> a
--
-- >>> think 21
-- 2
--
-- >>> think 42
-- 2
--
think :: Int -> Int
think x
    | x < 1 = 2 -- we need to handle the edge case!
    | otherwise = (((x + (1::Int))^(2::Int) - 1) `div` x ) - x
    -- why "(((x + 1)^2 - 1) `div` x ) - x" throws compiler Warnings?



-- | Replace old substring with new in text
--
-- >>> replace "X" "I" "XI"
-- "II"
--
-- >>> replace "X" "I" []
-- ""
--
-- >>> replace "IIIII" "V" "IIIIII"
-- "VI"
--
replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new text
    | length old > length text = text
    | otherwise =
        if take size text == old
            then new ++ replace old new (drop size text)
            else head text : replace old new (tail text)
        where
            size = length old



-- | converts an int into Roman literal
--
-- >>> toRoman 1
-- "I"
--
-- >>> toRoman 17
-- "XVII"
--
-- >>> toRoman 33
-- "XXXIII"
--
-- >>> toRoman 38
-- "XXXVIII"
--
-- >>> toRoman 4
-- "IV"
--
-- >>> toRoman 9
-- "IX"
--
-- >>> toRoman 8
-- "VIII"
--
toRoman :: Int -> String
toRoman n =
  replace "IIII" "IV" $
  replace "VIIII" "IX" $
  replace "LL" "C" $
  replace "XXXXX" "L" $
  replace "VV" "X" $
  replace "IIIII" "V" $
  replicate n 'I'



-- | Converts a list of numbers into a string row with numbers being comma separated.
--
-- >>> int3string [1,2,3]
-- "1, 2, 3"
--
-- >>> int3string []
-- ""
--
-- >>> int3string [1]
-- ""
--
int3string :: [Int] -> String
int3string xs
    | length xs == 3 = show (head xs) ++ ", " ++
            show (xs !! 1) ++ ", " ++
            show (xs !! 2)
    | otherwise = ""

-- better to do it this way:
-- int3string :: [a] -> String
-- int3string = print . concat . (intersperse ", ") . map show

-- | swap3 swaps first and third element of the list
--
-- >>> swap3old []
-- []
--
-- >>> swap3old [1,2]
-- []
--
-- >>> swap3old [1,2,3]
-- [3,2,1]
--
-- >>> swap3old [1,2,3,4]
-- []
--
-- Note, the compiler does not understand that the pattern
-- is exhaustive, because lists of different length than 3
-- are covered by the first guard. Annoying!
swap3old :: [Int] -> [Int]
swap3old [a, b, c] = [c, b, a]
swap3old _ = []

{-- This implementation is correct, more verbose, and throws
    a compiler warning about non-exhaustive pattern matching.

    | length xs /= 3 = []
    | otherwise = [c, b, a] where
      [a, b, c] = xs -- using destructuring and pattern matching. The pattern is non-exhaustive!
--}


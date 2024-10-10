{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Lib
  ( Age (..),
    addAge,
    addNumber,
    mhead,
    mhead2,
  )
where

-- [Task 3]

-- | addNumber sums two numbers.
--
-- I need to use the 'Num' type to specify that expected params are numbers.
--
-- >>> addNumber 1 2
-- 3
addNumber :: (Num a) => a -> a -> a
addNumber a b = a + b

-- | 'newtype' declares a new type of some base type (in this case 'Int').
-- Deriving basically 'adds' some properties to the new defined type,
-- 'Show' in this case, which allows it to be converted to string
-- and displayed to the console.
newtype Age = Age Int deriving (Show)

-- | Sums two Age instances.
--
-- From my understanding, when we want to use this function we have to use "pattern matching",
-- which means adding "(Age ...)" is needed to extract the underlying Int values from the Age type.
--
-- >>> addAge (Age 10) (Age 10)
-- Age 20
addAge :: Age -> Age -> Age
addAge (Age x) (Age y) = Age (x + y)

-- [Task 4]

-- | mhead is my own implementation of 'head' function.
--
-- >>> mhead [4,2,3]
-- WAS NOW Just 4
-- NOW Just 4
--
-- >>> mhead ['a','b','c']
-- Just 'a'
--
-- >>> mhead "Hello"
-- Just 'H'
mhead :: [a] -> Maybe a
mhead [] = Nothing
mhead (x : _) = Just x

-- | mhead2 is my own implementation of head function.
--
-- >>> mhead2 [1,2,3]
-- WAS 1
-- NOW Just 1
--
-- >>> mhead2 ['a','b','c']
-- WAS a
-- NOW Just 'a'
--
-- >>> mhead2 "Hello"
-- WAS H
-- NOW Just 'H'
mhead2 :: [a] -> Maybe a
mhead2 x = if null x then Nothing else Just (x !! 0)

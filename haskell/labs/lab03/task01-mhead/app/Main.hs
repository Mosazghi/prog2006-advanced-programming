{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Main (main) where

import Lib

main :: IO ()
main = someFunc

-- [Task 01]

-- | mhead1 returns the first element of a list
--
-- >>> mhead1 [1,2,3]
mhead1 :: [a] -> a
mhead1 [] = error "empty list"
mhead1 (x : _) = x

-- | mhead2 returns the first element of a list
--
-- >>> mhead2 [1,2,3]
mhead2 :: [a] -> a
mhead2 xs
  | null xs = error "empty list"
  | otherwise = xs !! 0

-- | mhead3 returns the first element of a list
--
-- >>> mhead3 [2,4,5]
mhead3 :: [a] -> a
mhead3 xs = if null xs then error "empty list" else xs !! 0

-- | mhead4 returns the first element of a list
mhead4 :: [a] -> a
mhead4 [] = error "empty list"
mhead4 xs =
  let t = xs
   in t !! 0

-- | mhead5 returns the first element of a list
mhead5 :: [a] -> a
mhead5 [] = error "empty list"
mhead5 xs = x
  where
    x = xs !! 0

-- | mhead6 returns the first element of a list
mhead6 :: [a] -> a
mhead6 xs = case xs of
  [] -> error "empty list"
  (x : _) -> x

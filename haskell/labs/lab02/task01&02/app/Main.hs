module Main (main) where

import Lib

main :: IO ()
main = print (mreverse "Hello") 
  -- putStrLn "Hello World"
  -- n <- getLine
  -- mulTable (read n :: Int)
  



-- | mreverse is my own implementation of list reversal
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse "World"
mreverse :: [a] -> [a]
mreverse [] = []
mreverse (x:xs) = mreverse xs ++ [x]


-- [Task 2]
-- | mulTable prints multiplication table of size n
--
-- >>> mulTable 3
-- 
mulTable :: Int -> IO () 
mulTable n = putStrLn (table ++ "\n")
  where
    table = unlines [unwords [padding (i * j) | j <- [1..n]] | i <- [1..n]]



-- | adds padding to the string
--
-- >>> padding 1
-- "  1"
padding :: Int -> String
padding num = replicate (3 - length (show num)) ' ' ++ show num

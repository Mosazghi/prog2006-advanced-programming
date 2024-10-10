module Main where

import Lib

main :: IO ()
main = getJoke >>= putStrLn
-- The same code with do notation 
--  do
--    joke <- getJoke
--    putStrLn joke

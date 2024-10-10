module Main (main) where

-- Lib needs to be imported here for the doctests in the 
-- Lib.hs file to run because the doctest command is run
-- on this file
import Lib

main :: IO ()
main = putStrLn "This is an example of a doctest and quick test set-up."

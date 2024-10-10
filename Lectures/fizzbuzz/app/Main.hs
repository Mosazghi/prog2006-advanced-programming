module Main (main) where

import Lib

main :: IO ()
main = do
    mapM_ (putStrLn . fizzBuzz) [1..20]
    mapM_ (putStrLn . fizzBuzz') [1..20]
    print $ convert [1, 2, 3]

module Main (main) where

import Lib (countScore)

main :: IO ()
main = do
    txt <- getContents
    putStrLn $ "The total score is: " ++ show (countScore txt)

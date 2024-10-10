module Main (main) where

import Lib 

main :: IO ()
main = do
  -- [Task 1 & 2 ] 
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "and what is your age?"
  age <- getLine
  let (Age ageIn10years) = addAge (Age (read age :: Int)) (Age 10)
  putStrLn $ "Hello " ++ name ++ ", in 10 years you will be " ++  show ageIn10years ++ "."
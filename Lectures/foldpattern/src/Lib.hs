module Lib
    ( someFunc
    ) where


fun1 :: Int -> Bool
fun1 x = x > 10

fun2 :: Int -> Bool
fun2 x = x < 10

fun3 :: Int -> Bool
fun3 x = x == 10

{-- 
   Code Smell
   DO NOT DO THAT
   DRY - Don't Repeat Yourself
someFunc2 :: IO ()
someFunc2 = do 
    let answer = []
    let answer2 = if fun1 10 
        then answer ++ "Bigger than 10" 
        else answer
    let answer3 = if fun2 10 
        then answer2 ++ "Smaller than 10" 
        else answer2
    let answer4 = if fun3 10 
        then answer3 ++ "Equal to 10" 
        else answer3
    putStrLn answer4 
--}

checks :: [(Int -> Bool, String)]
checks = [(fun1, "Bigger than 10"), 
          (fun2, "Smaller than 10"), 
          (fun3, "Equal to 10")]

someFunc :: IO ()
someFunc = putStrLn answer where
    answer = foldl processor "" checks
    processor acc (f, s) = if f 10 
                                then acc ++ s 
                                else acc

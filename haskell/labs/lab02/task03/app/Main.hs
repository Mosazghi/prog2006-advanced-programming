module Main (main) where
import Data.Char

-- import Lib
main :: IO ()
main = do
  contents <- getContents
  print (countMaxAge contents)


-- | Counts the number of max ages in a given data file 
countMaxAge :: Num b => String -> b
countMaxAge contents = 
  let  lineList = words contents
       numbersList = filter (all isNumber) lineList
       oldestAge = maximum numbersList
       res = (foldl (\acc x -> if x == oldestAge then acc + 1 else acc) 0 numbersList)
  in res 

module Lib
    ( myHeadFunction
    ) where

  
-- |   test myHeadFunction
-- >>> myHeadFunction [1,2,3]
-- 1
-- 
-- |   test myHeadFunction
-- >>> myHeadFunction [3,2,1]
-- 3
-- 
-- |   test myHeadFunction
-- >>> myHeadFunction ['a','b','c']
-- 'a'
-- 
  
myHeadFunction :: [a] -> a
myHeadFunction list = head list

module Lib
    ( mfact,
    fibs, fibs2    ) where

-- [Task 02] 

-- | mfact returns a factorial of a given integer
-- 
-- >>> mfact 5
-- 120
--
-- >>> mfact 6
-- 720
--
-- >>>  mfact 10
-- NOW 3628800
mfact :: Int -> Int 
mfact 0 = 1 
mfact n = product [1..n]


-- [Task 03] 

-- | fibs return the nth fib. sequence 
-- 
-- >>> fibs 7
--  
fibs :: Int -> Int 
fibs 0 = 0
fibs 1 = 1 
fibs n = fibs (n-1) + fibs (n-2)

-- [Task 04]
-- | fib2 returns the nth fib. seq. 
-- this is an infinite list, where it does lazy evaluation 
-- '(b:_)' retrieves the head of a list 
-- 't@(..)' is useful because it's the entire list  
fibs2 :: Int -> Int 
fibs2 n = fib2 !! n 

-- infinite fib. seq. list 
fib2 = [0,1] ++ next fib2 
  where 
    next (a : t@(b:_)) = (a+b) : next t

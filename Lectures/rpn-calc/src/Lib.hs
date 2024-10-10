module Lib
    ( processLine
    , processTokens
    , fib, fibF, fibN, fibFn
    ) where

import Text.Read (readMaybe)
import Control.Monad.State.Lazy (State, get, put, evalState, StateT, lift, liftIO)
import Control.Monad (replicateM)


-- This implementation takes adventage of StateT monad to keep track of the program state
-- Study it to get ideas how you can solve problems involving manipulation of state
-- by taking advantage of the State monad.



-- | Process a line of input, and produce a string output
--
processLine :: String -> String
processLine line = unwords $ map show $ evalState (processTokens $ words line) (False, [])


-- Our working stack is composed of either floats or errors (as strings)
-- [Either String Float]

type Stack = [Either String Float]
type ProgState = State (Bool, Stack) Stack


-- Process tokens (list of words) and update the program state
processTokens :: [String] -> ProgState
processTokens [] = do
  (_, stack) <- get
  return stack

processTokens (t:ts) = do
  (ignore, stack) <- get
  if t == "\"" then put (not ignore, stack) >> processTokens ts
               else if not ignore then (case t of
                                      "*" -> opMult
                                      "+" -> opAdd
                                      "pop" -> opPop
                                      _ -> opNum t) >> processTokens ts
                                  else processTokens ts


opMult :: ProgState
opMult = do
  (ignore, stack) <- get
  let new_stack = (if length stack < 2 
                    then do
                        Left "Not enough arguments for *" : stack
                    else do 
                        let a:b:rest = stack
                        ((*) <$> a <*> b) : rest)
  put (ignore, new_stack)
  return new_stack

opAdd :: ProgState
opAdd = do
  (ignore, stack) <- get
  let new_stack = (if length stack < 2 
                      then do
                          Left "Not enough arguments for +" : stack
                      else do 
                          let a:b:rest = stack
                          ((+) <$> a <*> b) : rest)
  put (ignore, new_stack)
  return new_stack

opPop :: ProgState
opPop = do
    (ignore, stack) <- get
    let new_stack = tail stack
    put (ignore, new_stack)
    return new_stack

opNum :: String -> ProgState
opNum token = do
    (ignore, stack) <- get
    let new_stack = case (readMaybe token :: Maybe Float) of
                        Nothing -> Left ("Parsing error, expected a number, got: " ++ token) : stack
                        Just n -> Right n : stack
    put (ignore, new_stack)
    return new_stack
    
    
    
    
-- ====================
-- Extras

-- | Infinite fibonacci sequence
-- 
-- >>> fibF !! 3
-- 2
-- 
-- >>> fibF !! 10
-- 55
-- 
-- >>> take 6 fibF
-- [0,1,1,2,3,5]
-- 
fibF :: (Num a) => [a]
fibF = 0 : rest where 
  rest = 1 : zipWith (+) fibF rest

-- | Returns nth element of the fibonacci sequence
fibFn :: (Num a) => Int -> a
fibFn num = fibF !! num  

-- | Alternative implementation of fibonacci computation
-- with the help of StateT monad.
-- (0, 1) -- (1,1) -- (1,2)  -- (2,3)  -- (3,5)
fib :: StateT (Integer, Integer) IO Integer
fib = do
  (a, b) <- get
  put (b, a + b)
  lift $ print (a + b)
  return (a + b)
  
-- | Returns nth element of the fibonacci sequence
fibN :: Int -> StateT (Integer, Integer) IO Integer
fibN num = do
  _ <- replicateM (num-1) fib
  (_, b) <- get
  return b
  

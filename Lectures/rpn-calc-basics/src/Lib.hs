module Lib
    ( processLine
    ) where

-- | Process a line of input.
-- We expect the input to be a well formed reverse polish notation expression.
processLine :: String -> String
processLine = show . rpn . words

-- | Evaluate a reverse polish notation expression.
rpn :: [String] -> Int
rpn = head . foldl f [] where
  f (x:y:ys) "+" = (x + y):ys
  f (x:y:ys) "*" = (x * y):ys
  f xs y = read y:xs
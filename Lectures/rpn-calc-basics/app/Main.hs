module Main (main) where

import Lib



main :: IO ()
main = interact $ unlines . map processLine . lines
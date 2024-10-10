{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( parseToList,
    calc,
    writeToFile,  )
where

-- !! NOTE: I HAVE NOT CHANGED THE IMPLEMENTATION OF THE FUNCTIONS !!
-- !! AS I HAVE BEEN FOCUSING ON IMPLEMENTING IT IN RUST INSTEAD. 
-- !! JAR-ROUNDING IS NOT IMPLEMENTED HERE. 

-- | Parse a string into a list of doubles
-- >>> parseToList "1.0\n2.0\n3.0"
-- [1.0,2.0,3.0]
-- >>> parseToList "0.0\n0.0\n0.0"
-- [0.0,0.0,0.0]
-- >>> parseToList "1.0\n2.0\n3.0\n4.0"
-- [1.0,2.0,3.0,4.0]
parseToList :: String -> [Double]
parseToList content = map read $ lines content

-- | Calculate the x%, keeping the result as a formatted string
-- >>> calc 0.3 [1.0, 2.0, 3.0]
-- ["0.3","0.6","0.9"]
-- >>> calc 0.3 [0.0, 0.0, 0.0]
-- ["0.0","0.0","0.0"]
-- >>> calc 0.7 [1.0, 2.0, 3.0, 4.0]
-- ["0.7","1.4","2.1","2.8"]
calc :: (Num b) => b -> [b] -> [b]
calc x = map (* x)

-- | Write the fees to a file
writeToFile :: (Show a) => FilePath -> [a] -> IO ()
writeToFile filePath list = writeFile filePath $ unlines (map show list)

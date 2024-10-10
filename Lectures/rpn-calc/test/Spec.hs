-- For the Property Testing we need the following language extension
{-# LANGUAGE TemplateHaskell #-}
-- And for our negative literals this comes really useful
{-# LANGUAGE NegativeLiterals #-}

import Test.DocTest(doctest)

main :: IO ()
main = do
  -- Let us run first all the doctests from our source files
  doctest ["-isrc", "app/Main.hs"]
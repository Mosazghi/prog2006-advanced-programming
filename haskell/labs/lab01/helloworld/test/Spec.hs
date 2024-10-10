{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck 
import Lib (mhead, mhead2)  -- replace YourModuleName with the name of the module where addAge is defined

main :: IO ()
main = hspec $ do 
  describe "mhead2" $ do
    it "should retrieve the first element" $ 
      property $ \(xs :: [Int]) -> not (null xs) ==> mhead2 xs == Just (head xs)

  describe "mhead" $ do
    it "should retrieve the first element" $ 
      property $ \(xs :: [Int]) -> not (null xs) ==> mhead xs == Just (head xs)

{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck 
import Lib (mfact)
main :: IO ()
main = hspec $ do 
    describe "mmfact" $ do
      it "should return a factorial of a given integer " $ 
        property $ \(x :: Int) -> mfact x  == factorial x 


factorial 0 = 1  
factorial n = n * factorial (n - 1)  
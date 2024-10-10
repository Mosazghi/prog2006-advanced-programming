import Test.DocTest(doctest)
import Test.QuickCheck(property)
import Test.Hspec(Spec, hspec, describe, shouldBe, it)

-- We need to import all our functions for tests
import Lib

-- | Unit and property tests for our files
testLectureThree :: Spec
testLectureThree =
    describe "Various tests for Lib module" $ do
      describe "swap'" $ do
        it "works for (2, 3)" $ do
            swap' (2::Int, 3::Int) `shouldBe` (3,2)
        it "handles case of different types too" $ do
            swap' (1::Int, "hello") `shouldBe` ("hello", 1)

        it "actually handles ANY case you throw in, eg. (String, Int)" $ property $
          \(x, y) -> swap' (x::String, y::Int) == (y, x)
        it "... or (Int, String)" $ property $
                  \(x, y) -> swap' (x::String, y::Int) == (y, x)

      -- example of property testing
      describe "sum' implementation" $ do
        it "gives us the same output as original sum" $ property $
          \x -> (sum' x :: Int) == sum x



main :: IO ()
main = do
  -- Let us run first all the doctests from our source files
  doctest ["-isrc", "app/Main.hs"]
  hspec $ do
    testLectureThree

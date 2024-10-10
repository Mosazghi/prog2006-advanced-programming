import Test.DocTest(doctest)
import Test.QuickCheck(property)
import Test.Hspec(hspec, describe, shouldBe, it)

-- Lib must be imported here because the quick test uses 
-- the myHeadFunction function
import Lib

main :: IO ()
main = do
  -- the command is run on the Main.hs file
  doctest ["-isrc", "app/Main.hs"]
  -- quick test set-up
  

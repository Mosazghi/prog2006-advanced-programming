import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Lib.hs", "app/Main.hs"]


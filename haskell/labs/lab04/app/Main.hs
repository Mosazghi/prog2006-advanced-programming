module Main (main) where
import Lib

main :: IO ()
main = do
    msg <- getContents
    print (words msg)
    case decodeMessage msg of
        Just n -> putStrLn $ "The message is " ++ show n ++ " characters long."
        Nothing -> putStrLn "The message cannot be decoded. Interference detected."

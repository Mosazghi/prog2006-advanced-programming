module Lib
    ( someFunc
    , someFunc2
    , ourAdd
    , ourId
    , ourCompose
    , ourSeq
    ) where

someFunc2 :: IO ()
someFunc2 = getLine >>= putStrLn . ("Hello " ++) 

ourAdd :: Maybe Int -> Maybe Int
ourAdd n = do
    num <- n -- ?? 
    return (num + 20)

    {--
    do
      name <- getLine
      let hello = "Hello " ++ name
      putStrLn hello

    do
    name <- getLine
    putStrLn $ "Hello " ++ name

    --}

-- identity function
ourId :: a -> a
ourId f = f

-- composition function
ourCompose :: (a -> b) -> (b -> c) -> (a -> c)
ourCompose f g = g . f

ourSeq :: Monad m => m a -> m b -> m b
ourSeq f g = f >> g




someFunc :: IO ()
someFunc = myif ((10::Int) > (20::Int)) (putStrLn "10 > 20") (putStrLn "Hello")

myif :: Bool -> a -> a -> a
myif True tc _= tc
myif False _ fc = fc




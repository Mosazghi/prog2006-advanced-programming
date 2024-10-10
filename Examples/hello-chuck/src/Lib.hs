{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getJoke
    ) where

import Data.Functor ((<&>))
import Data.Aeson (FromJSON, ToJSON, fieldLabelModifier)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, httpJSON,
                            getResponseBody, getResponseStatus)
import Network.HTTP.Types.Status ( Status(statusCode) )

-- | Constant for the API
chuckAPI :: Request
chuckAPI = "http://api.chucknorris.io/jokes/random"

--  | Joke is the struct used to  unmarshal the JSON response from the URL
data Joke = Joke { categories:: [String]
                 , icon_url  :: String
                 , id        :: String
                 , url       :: String
                 , value     :: String
                 } deriving (Show, Generic)



instance FromJSON Joke
-- instance ToJSON Joke -- we do not use it, so we do not generate it.

-- | Main HTTP API fetching logic.
--   http-conduit offers a high-level API that does JSON parsing "by default"
--   and returns @IO <type>@ that has been parsed. We fetch the response,
--   parse it, extract value (the content of the joke) and return it as IO String.
getJoke :: IO String
getJoke = httpJSON chuckAPI <&> value . getResponseBody


{--
    We did this implementation, with <&> from Data.Functor, because it is the most idiomatic Haskell way of achieving the task.

    httpJSON chuckAPI <&> value . getResponseBody

    The <&> takes adventage of the fact that httpJSON is already operating within the IO context,
    and the "value" function from the Joke record returns for us "String", which is our "content" of f
    Observe the type signature for <&>
        (<&>) :: Functor f => f a -> (a -> b) -> f b
    In our case, f is IO, and b is String because "value . getResponseBody" returns String



    Alteratively, we could do this implementation with the bind >>= operator:

    httpJSON chuckAPI >>= return . value . getResponseBody

    I personally did that in the past, and I like it. I think it is almost
    as perfect as the one with <&>.



    Or, of course, we could also do this, with FMAP, in infix (<$>) operator:

    value . getResponseBody <$> httpJSON chuckAPI



    Notes on error handling.

    The above implementation handles errors as panics through the HTTP.Simple library.
    If you were to handle errors yourself, you would need to implement the logic and
    checks yourself, using lower level APIs.
-}

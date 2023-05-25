{-
This is the main program of Irminsul API server.
It will listen to port 50000 to handle API requests.
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Data.JSON

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Network.HTTP.Types

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Irminsul API Server starts on port 50000..."

    run 50000
    -- Uncomment this to show all requests
    -- $ logStdoutDev
        app

{-
Wai enforces us to respond in a lazy string.
And that's why there's a long sequence of string conversions.
-}
showResponse :: Show a => a -> Response
showResponse =
    -- HTTP header
    responseLBS status200 [("Content-Type", "text/plain")] .
    -- String conversions
    BSL.pack . BS.unpack . E.encodeUtf8 . T.pack . show

unpackPath :: [T.Text] -> [String]
unpackPath = (T.unpack <$>)

{-
Here defines all the possible paths of API requests.
-}
app :: Application
app req respond = do
    case pathInfo req of
        {-
        Relation Graph
        Path: api/relation-graph/<cluster-id>/<language-code>
        -}
        path@["api", "relation-graph", _, _] ->
            let [_, _, id, lang]    = unpackPath path in
            respond . showResponse $ apiRelationGraph id lang

        {-
        It goes here if the request doesn't match any of the paths above.
        -}
        _ -> respond $ showResponse apiUnknown

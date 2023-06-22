{-
This is the main program of Irminsul API server.
It will listen to port 50000 to handle API requests.
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import API

import Network.Wai ( responseLBS, Request(pathInfo), Response, Application )
import Network.Wai.Middleware.RequestLogger ()
import Network.Wai.Handler.Warp ( run )
import Network.HTTP.Types ( status200 )

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

{-
Here defines all the possible paths of API requests.
-}
app :: Application
app req respond =
    case (T.unpack <$> pathInfo req) of
        {-
        Relation Graph
        Path: api/relation-graph/<cluster-id>/<language-code>
        -}
        ["api", "relation-graph", cluster_id, lang] ->
            respond . showResponse $ apiRelationGraph cluster_id lang

        {-
        It goes here if the request doesn't match any of the paths above.
        -}
        _ -> respond $ showResponse apiUnknown

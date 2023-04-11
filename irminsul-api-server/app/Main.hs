{-# LANGUAGE OverloadedStrings #-}
module Main where

import Query ( showToUtf8LazyByteString, apiRelationGraph, apiResponse, ApiStatusCode(..) )
import Data.JSON

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.Text as T

textResponse = responseLBS status200 [("Content-Type", "text/plain")]
unpackPath :: [Text] -> [String]
unpackPath = (T.unpack <$>)

main :: IO ()
main = do
    putStrLn "Irminsul API Server starts on port 50000..."

    run 50000
    -- Uncomment this to show all requests
    -- $ logStdoutDev
        app

app :: Application
app req respond = do
    case pathInfo req of

        path@["api", "relation-graph", _, _] -> do
            let [_, _, id, lang] = unpackPath path
            let json = apiRelationGraph id lang
            respond $ textResponse (showToUtf8LazyByteString json)

        _ -> respond $ textResponse (showToUtf8LazyByteString (apiResponse UnknownApi JNull))

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Query
import LanguagePack

import Data.String

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.Text as T

textResponse = responseLBS status200 [("Content-Type", "text/plain")]
unpackPath :: [Text] -> [String]
unpackPath = (T.unpack <$>)

main :: IO ()
main = run 50000
    -- Uncomment this to show all requests
    -- $ logStdoutDev
    app

app :: Application
app req respond = do
    case pathInfo req of

        [] -> respond $ textResponse "1"

        path@["api", "relation-graph", _, _] -> do
            let [_, _, id, lang] = unpackPath path
            let json = apiRelationGraph id lang
            respond $ textResponse (showToUtf8LazyByteString json)

        _ -> respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found"

{-
This is the main program of Irminsul API server.
It will listen to port 50000 to handle API requests.
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import API

import Network.Wai ( responseLBS, Request(pathInfo), Response, Application )
import Network.Wai.Handler.Warp ( run )
import Network.HTTP.Types ( status200 )

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)

main :: IO ()
main = do
    args <- getArgs
    if null args then runOn 50000
    else do
        let portMaybe = readMaybe (head args) :: Maybe Int
        if isNothing portMaybe then putStrLn "Cannot read port number."
        else do
            let port = fromJust portMaybe
            if port < 0 || port > 65535 then putStrLn "Invalid port number."
            else runOn port
    where 
        runOn port = do
            putStrLn $ "Irminsul API starts on port " ++ show port ++ "..."
            run port app

{-
Here defines all the possible paths of API requests.
-}
app :: Application
app req respond = respond $ case T.unpack <$> pathInfo req of
    "api" : params -> router params
    _ -> showResponse $ illegalRequest UnknownApi

router :: [String] -> Response
router params = showResponse $ case params of
    {-
    Relation Graph
    Path: api/relation-graph/<cluster-id>/<language-code>
    -}
    "relation-graph" : [clusterId, lang, whoAmI] -> apiRelationGraph clusterId lang whoAmI
    "relation-graph" : [clusterId, lang]         -> apiRelationGraph clusterId lang "aether"
    "relation-graph" : [clusterId]               -> apiRelationGraph clusterId "en-us" "aether"
    "relation-graph" : _                         -> illegalRequest (ParamNumMismatch 3)

    -- It goes here if the request doesn't match any of the paths above.
    _ -> illegalRequest UnknownApi

{-
Wai enforces us to respond in a lazy string.
And that's why there's a long sequence of string conversions.
-}
showResponse :: Show a => a -> Response
showResponse = httpHeader . stringConversions . show where
    httpHeader = responseLBS status200 [("Content-Type", "text/plain")]
    stringConversions = BSL.pack . BS.unpack . E.encodeUtf8 . T.pack

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Query
import LanguagePack

import Web.Scotty
import Data.String

main = server

stringResponse :: String -> ActionM ()
stringResponse = text . fromString

dataResponse :: Show a => a -> ActionM ()
dataResponse = stringResponse . show

server = scotty 50000 $ do
    
    get "/api/test" $
        stringResponse "Hello, World! 你好世界！"

    get "/api/kof-demo" $
        dataResponse apiKofDemo
    
    get "/api/cluster" $ do
        id <- param "id"
        lang <- param "lang"
        dataResponse $ apiCluster id lang

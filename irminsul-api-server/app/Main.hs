{-# LANGUAGE OverloadedStrings #-}
module Main where

import Query
import LanguagePack

import Web.Scotty
import Data.String

main = scotty 50000 $ do

    get "/api/hello" $
        stringResponse "Hello, World!"
    
    get "/api/kof-demo" $
        dataResponse apiKofDemo

    get "/api/entity-relations" $
        (do
            id <- param "id"
            lang <- param "lang"
            dataResponse $ apiEntityRelations id lang
        )
        `rescue` const missingParameter

    get "/api/relation-graph" $
        (do
            id <- param "id"
            lang <- param "lang"
            dataResponse $ apiRelationGraph id lang
        )
        `rescue` const missingParameter

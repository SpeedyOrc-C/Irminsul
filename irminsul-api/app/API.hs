{-
All the APIs are defined here.
-}
module API (apiRelationGraph, apiUnknown) where

import Irminsul
import LanguagePack
import Root

import Data.JSON
import Data.Maybe

import API.RelationGraph

{-
API responses with a JSON that include a status code and a body.
This function embed the status code and the body into a new JSON.
-}
data ApiStatusCode
    = OK
    | MissingParameter
    | UnsupportedLanguage
    | NotImplementedCluster
    | NotImplementedEntity
    | LayoutMissing
    | UnknownApi
    deriving (Eq, Show)

apiResponse :: ApiStatusCode -> JSON -> JSON
apiResponse status body =
    JObject [
        ("status", JString $ show status),
        ("body", body)]


{- |
Try to find a entity that matches the input ID.
-}
entityFromId :: String -> Maybe Entity
entityFromId "Root" = Just root
entityFromId inputId =
    let result = filter (\e -> entityId e == inputId)
            (elements . entities $ root) in
    if null result
    then Nothing
    else Just (head result)

{- APIs -}

apiRelationGraph :: String -> String -> JSON
apiRelationGraph inputId inputLang =
    maybe (apiResponse UnsupportedLanguage JNull)
    (\language ->
        maybe (apiResponse NotImplementedCluster JNull)
        (\entity ->
            if isNothing (layout entity)
            then apiResponse LayoutMissing JNull
            else apiResponse OK $ relationGraph language entity
        )
        (entityFromId inputId)
    )
    (readLanguageCode inputLang)

-- | Response with this if the input is illegal.
apiUnknown :: JSON
apiUnknown = apiResponse UnknownApi JNull

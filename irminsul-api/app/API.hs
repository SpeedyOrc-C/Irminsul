{-
All the APIs are defined here.
-}
module API (apiRelationGraph, illegalRequest, ApiStatusCode (..)) where

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
    | ParamNumMismatch { paramNumRequired :: Integer }
    | UnsupportedLanguage
    | NotImplementedCluster
    | NotImplementedEntity
    | LayoutMissing
    | UnknownApi
    | InvalidTravellerName
    deriving (Eq, Show)

apiResponse :: ApiStatusCode -> JSON -> JSON
apiResponse status body =
    JObject [
        ("status", JString $ show status),
        ("body", body)]

-- |　Illegal API request.
illegalRequest :: ApiStatusCode -> JSON
illegalRequest = (`apiResponse` JNull)

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

apiRelationGraph :: String -> String -> String -> JSON
apiRelationGraph inputId inputLang inputWhoAmI = let
    maybeId = entityFromId inputId
    maybeLang = readLanguageCode inputLang
    maybeWhoAmI = readWhoAmI inputWhoAmI

    response Nothing _ _ = illegalRequest NotImplementedCluster
    response _ Nothing _ = illegalRequest UnsupportedLanguage
    response _ _ Nothing = illegalRequest InvalidTravellerName
    response (Just entityId) (Just lang) (Just whoAmI) =
        if isNothing (layout entityId) then illegalRequest LayoutMissing
        else apiResponse OK $ relationGraph lang entityId whoAmI

    in response maybeId maybeLang maybeWhoAmI
    
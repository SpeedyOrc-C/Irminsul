module Query where

import Irminsul
import Root

import Data.List (intercalate, isInfixOf)
import Data.Char (toLower)

newtype QueryResult = QueryResult [Path]

instance Show QueryResult where
  show (QueryResult paths) = show paths

data QueryConfiguration = QueryConfiguration {
    partialMatch :: Bool,
    caseSensitive :: Bool
}

defaultQuery = QueryConfiguration {
    partialMatch = False,
    caseSensitive = False
}

fuzzyQuery = QueryConfiguration {
    partialMatch = True,
    caseSensitive = False
}

searchPath :: String -> Entity -> QueryConfiguration -> QueryResult
searchPath id cluster config =
    QueryResult $ (\(Path path) -> Path $ drop 1 path) <$> q id cluster (Path [root]) [] where
    
    match :: String -> String -> Bool
    match a b
        | partialMatch config && not (caseSensitive config) =
            map toLower a `isInfixOf` map toLower b
        | partialMatch config && caseSensitive config =
            a `isInfixOf` b
        | not (partialMatch config) && caseSensitive config =
            a == b
        | otherwise =
            map toLower a == map toLower b

    q :: String -> Entity -> Path -> [Path] -> [Path]
    q id atom@(Atom atomId _) nowPath paths
        | match id atomId = paths ++ [nowPath]
        | otherwise = paths

    q id cluster@(Cluster clusterId _ childClusters _ _) nowPath paths
        | match id clusterId = paths ++ [nowPath]
        | otherwise = concat $
            [q id childClusters (nowPath <> Path [childClusters]) paths
            | childClusters <- indices childClusters]

fuzzyRoot :: String -> QueryResult
fuzzyRoot "" = QueryResult []
fuzzyRoot id = searchPath id root fuzzyQuery

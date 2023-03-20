module Query where

import Irminsul
import Data.List (intercalate)
import Root

type Path = [Entity]
newtype QueryResult = QueryResult [Path]

instance Show QueryResult where
  show :: QueryResult -> String
  show (QueryResult paths) =
    intercalate "\n" [
        intercalate " > "
            (entityIdentifier <$> path) | path <- paths]

searchPath :: String -> Entity -> QueryResult
searchPath id cluster = QueryResult $ q id cluster [root] [] where
    q :: String -> Entity -> Path -> [Path] -> [Path]
    q id atom@(Atom atomId _) nowPath paths
        | id == atomId = paths ++ [nowPath]
        | otherwise = paths
    q id cluster@(Cluster clusterId _ childClusters _) nowPath paths
        | id == clusterId = paths ++ [nowPath]
        | otherwise = concat $
            [q id childClusters (nowPath ++ [childClusters]) paths
            | childClusters <- indices childClusters]

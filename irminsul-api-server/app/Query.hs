module Query where

import Irminsul
import LanguagePack
import Translation
import Root ( root )
import Root.Teyvat.Mondstadt.KnightsOfFavonius (knightsOfFavonius)

import Data.Maybe
import Data.String
import Debug.Trace
import Data.List
import Data.Vector
import Data.JSON

import Web.Scotty

traceThis :: Show a => a -> a
traceThis x = trace (show x) x

{-
    Wrapper of APIs' responses
-}
data ApiStatusCode
    = OK
    | MissingParameter
    | UnsupportedLanguage
    | NotImplementedCluster
    | NotImplementedEntity
    deriving (Eq, Show)

apiResponse :: ApiStatusCode -> JSON -> JSON
apiResponse status body =
    JObject [("status", JString (show status)), ("body", body)]

stringResponse :: String -> ActionM ()
stringResponse = text . fromString

dataResponse :: Show a => a -> ActionM ()
dataResponse = stringResponse . show

missingParameter =
    dataResponse (JObject [("status", JString (show MissingParameter))])

getAllPathsOfRoot :: [(Entity, Path)]
getAllPathsOfRoot = getAllPaths root (Path [])

getAllPaths :: Entity -> Path -> [(Entity, Path)]
getAllPaths entity (Path nowPath) = (entity, Path nowPath) : do
    let clusters = filter isCluster (indices . entities $ entity)
    cluster <- clusters
    getAllPaths cluster (Path (nowPath ++ [entity]))

showPaths :: [(Entity, Path)] -> IO ()
showPaths paths = putStrLn $ intercalate "\n" $
    (\(entity, path) -> show path ++ " - " ++ entityId entity) <$> paths

{-
    Implementation of APIs
-}

-- | Relations grouped in forward, backward and bidirectional ones.
type GroupedRelations = ([Relation], [Relation], [Relation])

clusterFromId :: String -> Maybe Entity
clusterFromId "Root" = Just root
clusterFromId id =
    let result = filter (\e -> isCluster e && entityId e == id)
            (elements . entities $ root) in
    if null result
    then Nothing
    else Just (head result)

entityFromId :: String -> Maybe Entity
entityFromId "Root" = Just root
entityFromId id =
    let result = filter (\e -> entityId e == id)
            (elements . entities $ root) in
    if null result
    then Nothing
    else Just (head result)

splitAtomsClusters :: [Entity] -> ([Entity], [Entity])
splitAtomsClusters xs = (filter isAtom xs, filter isCluster xs)

-- | Group all relations between 2 entities.
splitRelationsBetween ::
    Entity -> Entity -> [Relation] -> GroupedRelations
splitRelationsBetween s o relations = (
    filter (\r -> matchSubject s r && matchObject o r) relations,
    filter (\r -> matchSubject o r && matchObject s r) relations,
    swapBiRelationSubject s <$> filter isBiRelation relations
    )

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (a, b) = if a <= b then (a, b) else (b, a)

clusterGraph :: Language -> Entity -> JSON
clusterGraph _ (Atom {}) = JNull
clusterGraph _ (Cluster rootId _ _ _ Nothing) = JNull
clusterGraph lang
    cluster@(Cluster
        rootId _ _ _
        (Just layout@(RelationGraphLayout rootLayout entityLayouts))) =
    let
        renderedEntities = filter
            -- Keep entities which has their layouts defined
            (`elem` (fst <$> entityLayouts))
            (elements . entities $ cluster)

        renderedRelations =
            filter
            (\r ->
                let (e1, e2) = subjectAndObject r in
                -- Keep relations which has subject and object in rendered
                -- entities, so that we can calculate relation's layout
                e1 `elem` renderedEntities && e2 `elem` renderedEntities ||
                -- Don't forget the root cluster is an exception,
                -- it can also be in a relation
                e1 == cluster && e2 `elem` renderedEntities ||
                e2 == cluster && e1 `elem` renderedEntities
            )
            (elements . relations $ cluster)

        (renderedAtoms, renderedClusters) = splitAtomsClusters renderedEntities

        lp = getLanguagePack lang
    in
    JObject [
        ("id", JString rootId),
        ("rootPosition", toJSON (position rootLayout)),

        ("rootTranslation", JString $ translateEntity lp cluster),

        ("atoms", JArray $ do
            a <- renderedAtoms
            let maybeLayout = lookup a entityLayouts
            if isJust maybeLayout then do
                let layout = fromJust maybeLayout
                return $ JObject [
                        ("id", JString (entityId a)),
                        ("translation", JString (translateEntity lp a)),

                        ("position", toJSON (position layout))
                    ]
            else []
        ),

        ("clusters", JArray $ do
            c <- renderedClusters
            let maybeLayout = lookup c entityLayouts
            if isJust maybeLayout then do
                let layout = fromJust maybeLayout
                return $ JObject [
                        ("id", JString $ entityId c),
                        ("translation", JString $ translateEntity lp c),

                        ("position", toJSON $ position layout),
                        ("anchor", toJSON $ anchor layout),
                        ("size", toJSON $ size layout)
                    ]
            else []
        ),

        ("relationsBetween", JArray $ do
            relationGroup <- groupBy sameEntityPair
                (elements . relations $ cluster)

            let (subject, object) =
                    sortTuple $ subjectAndObject $ head relationGroup

            let (forwardRelations, backwardRelations, biRelations) =
                    splitRelationsBetween subject object relationGroup

            if  subject `elem` (fst <$> entityLayouts) &&
                object `elem` (fst <$> entityLayouts)
                ||
                subject == cluster &&
                object `elem` (fst <$> entityLayouts)
                ||
                object == cluster &&
                subject `elem` (fst <$> entityLayouts)
            then
                return $ JObject [
                    ("subjectId", JString $ entityId subject),
                    ("objectId", JString $ entityId object),

                    ("forwardRelations", JArray $ JString .
                        translateAction lp . action <$> forwardRelations),
                    ("backwardRelations", JArray $ JString .
                        translateAction lp . action <$> backwardRelations),
                    ("biRelations", JArray $ JString .
                        translateAction lp . action <$> biRelations)
                    ]
            else []
        )
    ]

-- | Group all relations related to the specified cluster.
splitRelationsOf :: Entity -> [Relation] -> GroupedRelations
splitRelationsOf s relations = (
    filter (matchSubject s) relations,
    filter (matchObject s) relations,
    filter (\r -> isBiRelation r && (subject r == s || object r == s)) relations
    )

entityRelations :: Language -> Entity -> JSON
entityRelations lang entity =
    let
        lp = getLanguagePack lang

        (asSubject, asObject, asBoth) =
            splitRelationsOf entity (elements . relations $ root)
    in
    JObject [
        ("id", JString (entityId entity)),
        ("translation", JString (translateEntity lp entity)),

        ("asSubject", JArray $ (\(Relation action _ object) -> 
            JObject [
                ("id", JString (entityId object)),
                ("translation", JString (translateEntity lp object)),
                ("action", JString (translateAction lp action))
            ]
            ) <$> asSubject),
        
        ("asObject", JArray $ (\(Relation action subject _) -> 
            JObject [
                ("id", JString (entityId subject)),
                ("translation", JString (translateEntity lp subject)),
                ("action", JString (translateAction lp action))
            ]
            ) <$> asObject),
        
        ("asBoth", JArray $ (\(BiRelation action _ object) -> 
            JObject [
                ("id", JString (entityId object)),
                ("translation", JString (translateEntity lp object)),
                ("action", JString (translateAction lp action))
            ]
            ) . swapBiRelationSubject entity <$> asBoth)
    ]

{-
APIs
-}

{- |
    Given a cluster id and a specified language,
    returns the layout of the cluster.
-}
apiClusterGraph :: String -> String -> JSON
apiClusterGraph id lang =
    maybe (apiResponse UnsupportedLanguage JNull)
    (\language ->
        maybe (apiResponse NotImplementedCluster JNull)
        (apiResponse OK . clusterGraph language)
        (clusterFromId id)
    )
    (readLanguageCode lang)

{- |
    Given an entity id and a specified language,
    returns all the relations associated to this entity.
-}
apiEntityRelations :: String -> String -> JSON
apiEntityRelations id lang =
    maybe (apiResponse UnsupportedLanguage JNull)
    (\language ->
        maybe (apiResponse NotImplementedEntity JNull)
        (apiResponse OK . entityRelations language)
        (entityFromId id)
    )
    (readLanguageCode lang)


apiKofDemo = clusterGraph ZhCn knightsOfFavonius

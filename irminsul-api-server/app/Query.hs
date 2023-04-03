module Query where

import Irminsul
import Root

import Data.List
import Data.Vector
import Data.JSON
import LanguagePack
import Translation
import Data.Maybe
import Debug.Trace (trace)
import Root.Teyvat.Mondstadt.KnightsOfFavonius (knightsOfFavonius)

data ApiStatusCode
    = OK
    | UnsupportedLanguage
    | NotImplementedCluster
    deriving (Eq, Show)

apiResponse :: ApiStatusCode -> JSON -> JSON
apiResponse status body =
    JObject [("status", JString (show status)), ("body", body)]

traceThis :: Show a => a -> a
traceThis x = trace (show x) x

newtype QueryResult = QueryResult [Path]

clusterFromId :: String -> Maybe Entity
clusterFromId "Root" = Just root
clusterFromId id =
    let result = filter (\e -> isCluster e && entityId e == id)
            (elements . entities $ root) in
    if null result
    then Nothing
    else Just (head result)

splitAtomsClusters :: [Entity] -> ([Entity], [Entity])
splitAtomsClusters xs = (filter isAtom xs, filter isCluster xs)

splitRelations ::
    Entity -> Entity -> [Relation] -> ([Relation], [Relation], [Relation])
splitRelations s o rs = (
    filter (\r -> matchSubject s r && matchObject o r) rs,
    filter (\r -> matchSubject o r && matchObject s r) rs,
    filter isBiRelation rs
    )

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (a, b) = if a <= b then (a, b) else (b, a)

layoutJsonFromCluster :: Language -> Entity -> JSON
layoutJsonFromCluster _ (Atom {}) = JNull
layoutJsonFromCluster _ (Cluster _ _ _ _ Nothing) = JNull
layoutJsonFromCluster lang
    cluster@(Cluster
        rootId _ _ _
        (Just layout@(Layout rootLayout entityLayouts))) =
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
                        ("width", JString . show $ width layout),
                        ("height", JString . show $ height layout)
                    ]
            else []
        ),

        ("relationsBetween", JArray $ do
            relationGroup <- groupBy sameEntityPair
                (elements . relations $ cluster)

            let (subject, object) =
                    sortTuple $ subjectAndObject $ head relationGroup

            let (forwardRelations, backwardRelations, biRelations) =
                    splitRelations subject object relationGroup

            let subjectAnchor =
                    if subject == cluster
                    then anchor rootLayout
                    else anchor . fromJust $
                        lookup subject entityLayouts

                objectAnchor =
                    if object == cluster
                    then anchor rootLayout
                    else anchor . fromJust $
                        lookup object entityLayouts

            let relationVector = objectAnchor - subjectAnchor

            let relationWidth =
                    magnitude relationVector

                relationPosition =
                    scale (1/2) (subjectAnchor + objectAnchor)

                relationRotation =
                    let angle = atan (y relationVector / x relationVector) in
                    if x subjectAnchor <= x objectAnchor
                    then angle
                    else angle + pi

                needReverse =
                    x objectAnchor < x subjectAnchor
                    ||
                    x objectAnchor == x subjectAnchor &&
                    y objectAnchor > y subjectAnchor

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
                        translateAction lp . action <$> biRelations),

                    ("width", JNumber relationWidth),
                    ("position", toJSON relationPosition),
                    ("rotation", JNumber relationRotation),

                    ("needReverse", JBool needReverse)
                    ]
            else []
        )
    ]

layoutJsonFromId :: String -> Language -> JSON
layoutJsonFromId id lang = maybe JNull
    (layoutJsonFromCluster lang) (clusterFromId id)


{-
APIs
-}

{- |
    Given a cluster id and a specified language,
    returns the layout of the cluster.
-}
apiCluster :: String -> String -> JSON
apiCluster id lang =
    maybe (apiResponse UnsupportedLanguage JNull)
    (\language ->
        maybe (apiResponse NotImplementedCluster JNull)
        (apiResponse OK . layoutJsonFromCluster language)
        (clusterFromId id)
    )
    (readLanguageCode lang)

apiKofDemo = layoutJsonFromCluster ZhCn knightsOfFavonius

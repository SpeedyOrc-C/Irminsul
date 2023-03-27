module Showcase where

import Data.Maybe
import Data.List
import System.Directory

import Irminsul
import Utility.Vector
import Utility.Html
import Root
import Translation
import LanguagePack

import Root.Teyvat.Mondstadt.KnightsOfFavonius
import Debug.Trace

data ClusterShowcase = ClusterShowcase {
    path :: Path,
    cluster :: Entity
}

instance Show ClusterShowcase where
    show (ClusterShowcase path entity) =
        "ClusterShowcase " ++ show path ++ " > " ++ entityId entity

flattenShowcases :: Entity -> [ClusterShowcase]
flattenShowcases = flattenShowcases' (Path []) where
    flattenShowcases' :: Path -> Entity -> [ClusterShowcase]
    flattenShowcases' _ (Atom {}) = []
    flattenShowcases' nowPath rootEntity =
        filter (isJust . layout . cluster) $
        ClusterShowcase nowPath rootEntity : do
            childEntity <-
                filter isCluster .
                indices . entities $ rootEntity
            flattenShowcases' (nowPath <> Path [rootEntity]) childEntity

showcaseOutputDirectory :: String -> ClusterShowcase -> String
showcaseOutputDirectory directory (ClusterShowcase (Path path) _) =
    directory ++ concatMap ("/" ++) (entityId <$> path)

showcaseOutputPath :: String -> Language -> ClusterShowcase -> String
showcaseOutputPath directory language showcase =
    showcaseOutputDirectory directory showcase
    ++ "/" ++ entityId (cluster showcase) ++ "-" ++ show language ++ ".html"

toRootFolder :: ClusterShowcase -> String
toRootFolder showcase =
    concat (replicate (depth (path showcase)) "../")

clusterChildLocalPath :: Language -> Entity -> String
clusterChildLocalPath language cluster =
    entityId cluster ++ "-" ++ show language ++ ".html"

showcaseHtml :: Language -> ClusterShowcase -> Xml
showcaseHtml
    language
    showcase@(
        ClusterShowcase _
            cluster@(Cluster
                rootId _ _ _
                (Just layout@(
                    Layout
                    rootLayout
                    entityLayouts)))) =

    html language (translateEntity languagePack cluster) [
        rootFolder ++ "style/cluster-showcase.css"
    ] [
        Tag "div" [
            ("id", "cluster-showcase"),
            ("style", "transform: translate(0, 0) scale(100%)")
        ] (
            clusterTags ++
            atomTags ++
            relationTags ++
            return (
                Tag "div" [
                    ("id", rootId),
                    ("class", "root-cluster"),
                    ("style", atomStyleFromVector (position rootLayout))
                ] [
                    TagClosing "img" [("src", rootFolder ++
                        "img/ui/background-cluster-root-title.png")],
                    Tag "div" [("class", "entity-name")] [
                        Text $ translateEntity languagePack cluster
                    ]
                ])
        ),
        Tag "script" [("src", rootFolder ++ "js/cluster-showcase.js")] []
    ]
    where
        rootFolder = toRootFolder showcase
        languagePack = getLanguagePack language

        renderedStuffFilter =
            if clusterType cluster == Organization
            then elements else indices

        renderedEntities =
            filter (`elemLayout` layout) . renderedStuffFilter $
                entities cluster

        atomTags = do
            entity <- filter isAtom renderedEntities
            let (Just property) = lookup entity (filter (isAtom.fst) entityLayouts)
            return $ atomHtml (translateEntity languagePack entity)
                rootFolder (position property) entity

        clusterTags = do
            entity <- filter isCluster renderedEntities
            let (Just clusterLayout) = lookup entity (filter (isCluster.fst) entityLayouts)
            return $ clusterHtml (translateEntity languagePack entity)
                language (entityId cluster) rootFolder clusterLayout entity

        renderedRelations =
            filter (\r ->
                let (a, b) = subjectAndObject r
                in a `elem` renderedEntities && b `elem` renderedEntities
                    -- Don't forget root cluster, the most special one!
                    || a == cluster && b `elem` renderedEntities
                    || b == cluster && a `elem` renderedEntities)
            . renderedStuffFilter $ relations cluster

        relationTags = do
            sameEntityPairs <- groupBy sameEntityPair renderedRelations
            let -- Don't forget root cluster here too!
                entityLayoutsWithRoot = entityLayouts ++ [(cluster, rootLayout)]
                (e1, e2) = subjectAndObject $ head sameEntityPairs
                (Just (ShowcaseElementProperty _ (Vector2 e1x e1y) _ _)) =
                    lookup e1 entityLayoutsWithRoot
                (Just (ShowcaseElementProperty _ (Vector2 e2x e2y) _ _)) =
                    lookup e2 entityLayoutsWithRoot

                midPoint = Vector2 ((e1x + e2x) / 2) ((e1y + e2y) / 2)
                reversed
                    | e1x < e2x = False
                    | e1x > e2x = True
                    | e1y > e2y = False
                    | otherwise = True
                rotation = atan ((e2y - e1y) / (e2x - e1x)) +
                    if e1x > e2x then pi else 0
                length = sqrt (((e2x - e1x)**2) + ((e2y - e1y)**2))

            return $ if reversed
                then relationHtml
                    midPoint length (rotation + pi) e1 e2
                    (translateAction languagePack . action <$>
                        filter isBiRelation sameEntityPairs)
                    (translateAction languagePack . action <$>
                        filter (matchSubject e2) sameEntityPairs)
                    (translateAction languagePack . action <$>
                        filter (matchSubject e1) sameEntityPairs)
                else relationHtml
                    midPoint length rotation e1 e2
                    (translateAction languagePack . action <$>
                        filter isBiRelation sameEntityPairs)
                    (translateAction languagePack . action <$>
                        filter (matchSubject e1) sameEntityPairs)
                    (translateAction languagePack . action <$>
                        filter (matchSubject e2) sameEntityPairs)

toRem :: (Eq a, Num a, Show a) => a -> String
toRem 0 = "0"
toRem x = show x ++ "rem"

atomHtml :: String -> String -> Vector2 -> Entity -> Xml
atomHtml nameLocal rootFolder position (Atom id _) =
    Tag "div" [
        ("id", id),
        ("class", "atom"),
        ("style", atomStyleFromVector position)
    ] [
        TagClosing "img"
            [("src", rootFolder ++ "img/avatar/" ++ id ++ ".png")],
        Tag "div" [("class", "entity-name")]
            [Text nameLocal]
    ]

atomStyleFromVector :: Vector2 -> String
atomStyleFromVector (Vector2 x y) =
    "left: " ++ toRem x
    ++ "; top: " ++ toRem (-y)

clusterHtml :: String -> Language -> String -> String -> ShowcaseElementProperty -> Entity -> Xml
clusterHtml nameLocal language thisFolder rootFolder property (Cluster id _ _ _ _) =
    Tag "div" [
        ("id", id),
        ("class", "cluster"),
        ("style", clusterStyleFromRectangle property)
    ] [
        Tag "a" [("class", "entity-name"), ("href",
            thisFolder ++ "/" ++ id ++ "-" ++ show language ++ ".html")]
            [Text nameLocal]
    ]
    where
        clusterStyleFromRectangle :: ShowcaseElementProperty -> String
        -- TODO: Anchor is ignored here yet as well
        clusterStyleFromRectangle (ShowcaseElementProperty (Vector2 x y) _ width height) =
            "left: " ++ toRem x
            ++ "; top: " ++ toRem (-y)
            ++ "; width: " ++ toRem width
            ++ "; height: " ++ toRem height

relationHtml ::
    Vector2 -> Double -> Double ->
    Entity -> Entity ->
    [String] -> [String] -> [String] -> Xml
relationHtml
    position width rotation
    from to
    biRelations forwardRelations backwardRelations =

    Tag "div" [
        ("id", entityId from ++ "-" ++ entityId to),
        ("class", "relation-between"),
        ("style", relationStyleFromStuff position width rotation)
    ] ([
        Tag "div" [("class", "bi-relation")]
            (putInDiv <$> biRelations) | not (null biRelations)] ++ [
        Tag "div" [("class", "forward-relation")]
            (putInDiv <$> forwardRelations) | not (null forwardRelations)] ++ [
        Tag "div" [("class", "backward-relation")]
            (putInDiv <$> backwardRelations) | not (null backwardRelations)
    ])
    where
        putInDiv x = Tag "div" [] [Text x]

        relationStyleFromStuff :: Vector2 -> Double -> Double -> String
        relationStyleFromStuff (Vector2 x y) width rotation =
            "left: " ++ toRem x
            ++ "; top: " ++ toRem (-y)
            ++ "; width: " ++ toRem width
            ++ "; transform: translate(-50%, -50%) rotate(" ++ show (-rotation) ++ "rad)"

createShowcaseDirectory :: String -> ClusterShowcase -> IO ()
createShowcaseDirectory directory s@(ClusterShowcase _ entity) = do
    let outputDirectory = showcaseOutputDirectory directory s
    createDirectoryIfMissing True outputDirectory



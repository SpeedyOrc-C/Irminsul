{-# LANGUAGE ViewPatterns #-}
module API.RelationGraph (relationGraph, readWhoAmI) where

import Irminsul
    ( Entity(Atom, Cluster, entityId, entities),
      Transform(position, Transform),
      Layout(Layout),
      IndexedSetFamily(IndexedSetFamily, indices),
      Relation(BiRelation, Relation, action),
      Path(..),
      isBiRelation,
      matchSubject,
      matchObject,
      swapBiRelationSubject,
      sameEntityPair,
      subjectAndObject,
      isCluster,
      isAtom )
import Translation
    ( Language,
      getLanguagePack,
      translateEntityWith,
      translateActionWith )
import Root ( root )

import Data.JSON ( ToJSON(toJSON), JSON(JObject, JNull, JArray, JString) )
import Data.Maybe ( fromJust )
import Data.List ( groupBy )
import Control.Applicative ( Alternative(empty) )
import Root.AnotherWorld (lumine, aether)
import Control.Arrow (Arrow(first))
import Prelude hiding (id)

{-|
    The players may choose Aether or Lumine when they play Genshin Impact
    at the first time.
-}
data WhoAmI = Aether | Lumine


readWhoAmI :: String -> Maybe WhoAmI
readWhoAmI "aether" = Just Aether
readWhoAmI "lumine" = Just Lumine
readWhoAmI _ = Nothing

{-|
    This replaces the traveller with Aether or Lumine based on
    players' preferences.

    For example, if you has chosen Aether, then you can see Aether in the
    relation graph of Knights of Favonius, and vice versa.
-}
replaceTraveller :: WhoAmI -> Entity -> Entity
replaceTraveller Aether (Atom "TravellerArchonSide" _) = aether
replaceTraveller Lumine (Atom "TravellerArchonSide" _) = lumine
replaceTraveller Aether (Atom "TravellerAbyssSide" _) = lumine
replaceTraveller Lumine (Atom "TravellerAbyssSide" _) = aether
replaceTraveller _ atom@(Atom {}) = atom

replaceTraveller whoAmI (Cluster entityId clusterType entities relations layout) =
    Cluster entityId clusterType
    (replaceEntity <$> entities)
    (replaceRelation <$> relations)
    (replaceLayout <$> layout)
    where
        replaceEntity :: Entity -> Entity
        replaceEntity = replaceTraveller whoAmI

        replaceRelation :: Relation -> Relation
        replaceRelation (BiRelation action s o) =
            BiRelation action (replaceEntity s) (replaceEntity o)
        replaceRelation (Relation action s o) =
            Relation action (replaceEntity s) (replaceEntity o)

        replaceLayout :: Layout -> Layout
        replaceLayout (Layout elementProperties rootProperty) =
            Layout
                elementProperties
                (first replaceEntity <$> rootProperty)

{-|
    This empowers the core functionality of the client.
    Providing a language code, a cluster id and who you are.
    It returns a graphical representation for this cluster.
-}
relationGraph :: Language -> Entity -> WhoAmI -> JSON
relationGraph lang notReplacedCluster
    ((`replaceTraveller` notReplacedCluster) ->
        cluster@(Cluster clusterId _
            (IndexedSetFamily _ allEntities)
            (IndexedSetFamily _ allRelations)
            (Just (Layout rootLayout entityLayouts))
        )
    )
    = let
    
    renderedEntities = filter (`elem` (fst <$> entityLayouts)) allEntities

    renderedAtoms    = filter isAtom    renderedEntities
    renderedClusters = filter isCluster renderedEntities

    languagePack = getLanguagePack lang
    translateEntity = translateEntityWith languagePack
    translateAction = translateActionWith languagePack

    jsonId = JString clusterId

    jsonPath = JArray $ do
        let (Path path) = fromJust $ lookup cluster allPathsOfRoot
        entity <- path
        return $ JObject [
            ("id",          JString $ entityId        entity),
            ("translation", JString $ translateEntity entity)]

    jsonRootPosition = toJSON $ position rootLayout

    jsonRootTranslation = JString $ translateEntity cluster

    jsonAtoms = JArray $ do
        renderedAtom <- renderedAtoms
        maybe empty (\layout -> return $ JObject [
            ("id",          JString $ entityId        renderedAtom),
            ("translation", JString $ translateEntity renderedAtom),
            ("position",    toJSON  $ position layout)
            ]) $ lookup renderedAtom entityLayouts

    jsonClusters = JArray $ do
        renderedCluster@(Cluster id _ (IndexedSetFamily _ allElements) _ _)
            <- renderedClusters
        maybe empty (\(Transform position anchor size) -> return $ JObject [
            ("id",          JString id),
            ("translation", JString $ translateEntity renderedCluster),
            ("elements",    JArray $ JString . entityId <$> allElements),
            ("position",    toJSON position),
            ("anchor",      toJSON anchor),
            ("size",        toJSON size)
            ]) $ lookup renderedCluster entityLayouts

    jsonRelationsBetween = JArray $ do
        relationGroup <- groupBy sameEntityPair allRelations

        let (subject, object) = sortTuple $ subjectAndObject $ head relationGroup

            (forwardRelations, backwardRelations, biRelations) =
                splitRelationsBetween subject object relationGroup

        if  subject `elem` renderedEntities && object `elem` renderedEntities
         || subject == cluster && object `elem` renderedEntities
         || object == cluster && subject `elem` renderedEntities
        then return . JObject $
            let translateActionsIn =
                    JArray . (JString . translateAction . action <$>)
            in [
            ("subjectId",   JString $ entityId subject),
            ("objectId",    JString $ entityId object),

            ("forwardRelations",  translateActionsIn forwardRelations),
            ("backwardRelations", translateActionsIn backwardRelations),
            ("biRelations",       translateActionsIn biRelations)]
        else empty

    jsonRelationGraph = JObject [
        ("id", jsonId),
        ("path", jsonPath),
        ("rootPosition", jsonRootPosition),
        ("rootTranslation", jsonRootTranslation),
        ("atoms", jsonAtoms),
        ("clusters", jsonClusters),
        ("relationsBetween", jsonRelationsBetween)
        ]

    in jsonRelationGraph

relationGraph _ _ _ = JNull


{-|
    Group all relations between 2 entities into categories of
    forward, backward and bidirectional ones.
-}
splitRelationsBetween ::
    Entity -> Entity -> [Relation] -> ([Relation], [Relation], [Relation])
splitRelationsBetween s o relations = (
    filter (\r -> s `matchSubject` r && o `matchObject` r) relations,
    filter (\r -> o `matchSubject` r && s `matchObject` r) relations,
    swapBiRelationSubject s <$> filter isBiRelation relations
    )


{- | Calculate the paths of all entities in the root. -}
allPathsOfRoot :: [(Entity, Path)]
allPathsOfRoot = getAllPaths root (Path []) where
    getAllPaths :: Entity -> Path -> [(Entity, Path)]
    getAllPaths entity (Path nowPath) = (entity, Path nowPath) : do
        let clusters = filter isCluster (indices . entities $ entity)
        cluster <- clusters
        getAllPaths cluster (Path (nowPath ++ [entity]))


sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (a, b) = if a <= b then (a, b) else (b, a)

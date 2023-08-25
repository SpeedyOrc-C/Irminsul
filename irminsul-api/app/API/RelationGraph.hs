{-
This empowers the core functionality of the client.
Providing a language code and a cluster id, return a layout for this cluster.
-}
module API.RelationGraph (relationGraph, readWhoAmI) where

import Irminsul
import Translation
import Root

import Data.JSON
import Data.Maybe ( fromJust )
import Data.List ( groupBy )
import Control.Applicative ( Alternative(empty) )
import Root.AnotherWorld (lumine, aether)
import Control.Arrow (Arrow(first))

{-|
    The players may choose Aether or Lumine when they play Genshin Impact
    at the first time. This function replace the traveller with
    Aether or Lumine based on their preferences.
-}
data WhoAmI = Aether | Lumine


readWhoAmI :: String -> Maybe WhoAmI
readWhoAmI "aether" = Just Aether
readWhoAmI "lumine" = Just Lumine
readWhoAmI _ = Nothing


replaceTraveller :: WhoAmI -> Entity -> Entity
replaceTraveller Aether (Atom {entityId="TravellerArchonSide"}) = aether
replaceTraveller Lumine (Atom {entityId="TravellerArchonSide"}) = lumine
replaceTraveller Aether (Atom {entityId="TravellerAbyssSide"}) = lumine
replaceTraveller Lumine (Atom {entityId="TravellerAbyssSide"}) = aether
replaceTraveller _ atom@(Atom {}) = atom

replaceTraveller whoAmI (Cluster entityId clusterType entities relations layout) =
    Cluster entityId clusterType 
    (IndexedSetFamily 
        (entityReplacer <$> indices entities) 
        (entityReplacer <$> elements entities)) 
    (IndexedSetFamily
        (relationReplacer <$> indices relations)
        (relationReplacer <$> elements relations)) 
    (layoutReplacer <$> layout)
    where
        entityReplacer = replaceTraveller whoAmI
        
        relationReplacer :: Relation -> Relation
        relationReplacer (BiRelation action s o) =
            BiRelation action (entityReplacer s) (entityReplacer o)
        relationReplacer (Relation action s o) =
            Relation action (entityReplacer s) (entityReplacer o)

        layoutReplacer :: RelationGraphLayout -> RelationGraphLayout
        layoutReplacer (RelationGraphLayout elementProperties rootProperty) =
            RelationGraphLayout
                elementProperties
                (first entityReplacer <$> rootProperty)


relationGraph :: Language -> Entity -> WhoAmI -> JSON
{-
These two inputs are illegal and should never happen.
But considering these cases for the sake of robustness.
-}
relationGraph _ (Atom {}) _ = JNull
relationGraph _ (Cluster _ _ _ _ Nothing) _ = JNull

{- Legal input goes here -}
relationGraph lang notReplacedCluster whoAmI = let
    cluster@(Cluster rootId _ _ _
        (Just (RelationGraphLayout rootLayout entityLayouts))) =
            replaceTraveller whoAmI notReplacedCluster

    renderedEntities = filter
        -- Keep entities which has their layouts defined
        (`elem` (fst <$> entityLayouts))
        (elements . entities $ cluster)

    renderedAtoms    = filter isAtom    renderedEntities
    renderedClusters = filter isCluster renderedEntities

    languagePack = getLanguagePack lang
    translateEntity = translateEntityWith languagePack
    translateAction = translateActionWith languagePack

    jsonId = JString rootId
    
    jsonPath = JArray $ do
        let (Path path) = fromJust $ lookup cluster allPathsOfRoot
        entity <- path
        return $ JObject [
            ("id",          JString $ entityId        entity),
            ("translation", JString $ translateEntity entity)]
    
    jsonRootPosition = toJSON (position rootLayout)

    jsonRootTranslation = JString $ translateEntity cluster

    jsonAtoms = JArray $ do
        renderedAtom <- renderedAtoms
        maybe empty (\layout -> return $ JObject [
            ("id",          JString $ entityId        renderedAtom),
            ("translation", JString $ translateEntity renderedAtom),
            ("position",    toJSON  $ position layout)
            ]) $ lookup renderedAtom entityLayouts
    
    jsonClusters = JArray $ do
        renderedCluster <- renderedClusters
        maybe empty (\layout -> return $ JObject [
            ("id",          JString $ entityId renderedCluster),
            ("translation", JString $ translateEntity renderedCluster),
            ("elements",    JArray $ JString .
                entityId <$> elements (entities renderedCluster)),
            ("position",    toJSON $ position   layout),
            ("anchor",      toJSON $ anchor     layout),
            ("size",        toJSON $ size       layout)
            ]) $ lookup renderedCluster entityLayouts

    jsonRelationsBetween = JArray $ do
        relationGroup <- groupBy sameEntityPair
            (elements . relations $ cluster)

        let (subject, object) =
                sortTuple $ subjectAndObject $ head relationGroup

            (forwardRelations, backwardRelations, biRelations) =
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
            return $ JObject $
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


{-| Group all relations between 2 entities into
    categories of forward, backward and bidirectional ones.
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

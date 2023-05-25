{-
This empowers the core functionality of the client.
Providing a language code and a cluster id, return a layout for this cluster.
-}
module API.RelationGraph (relationGraph) where

import Irminsul
import LanguagePack
import Translation
import Root

import Data.JSON
import Data.Maybe
import Data.List
import Control.Applicative

relationGraph :: Language -> Entity -> JSON
{-
These two inputs are illegal and should never happen.
But considering these cases for the sake of robustness.
-}
relationGraph _ (Atom {}) = JNull
relationGraph _ (Cluster rootId _ _ _ Nothing) = JNull

{- Legal input goes here -}
relationGraph lang
    cluster@(Cluster
        rootId _ _ _
        (Just layout@(RelationGraphLayout rootLayout entityLayouts))) =
    let
        renderedEntities = filter
            -- Keep entities which has their layouts defined
            (`elem` (fst <$> entityLayouts))
            (elements . entities $ cluster)

        renderedAtoms    = filter isAtom    renderedEntities
        renderedClusters = filter isCluster renderedEntities

        languagePack = getLanguagePack lang
        translateEntity = translateEntityWith languagePack
        translateAction = translateActionWith languagePack
    in
    JObject [
        ("id", JString rootId),

        ("path", JArray $ do
            let (Path path) = fromJust $ lookup cluster allPathsOfRoot
            entity <- path
            return $ JObject [
                ("id",          JString $ entityId        entity),
                ("translation", JString $ translateEntity entity)]
        ),

        ("rootPosition", toJSON (position rootLayout)),

        ("rootTranslation", JString $ translateEntity cluster),

        ("atoms", JArray $ do
            renderedAtom <- renderedAtoms

            maybe empty (\layout -> return $ JObject [
                ("id",          JString $ entityId        renderedAtom),
                ("translation", JString $ translateEntity renderedAtom),
                ("position",    toJSON  $ position layout)
                ]) $ lookup renderedAtom entityLayouts
        ),

        ("clusters", JArray $ do
            renderedCluster <- renderedClusters

            maybe empty (\layout -> return $ JObject[
                ("id",          JString $ entityId renderedCluster),
                ("translation", JString $ translateEntity renderedCluster),
                ("elements",    JArray $ JString .
                    entityId <$> indices (entities renderedCluster)),
                ("position",    toJSON $ position   layout),
                ("anchor",      toJSON $ anchor     layout),
                ("size",        toJSON $ size       layout)
                ]) $ lookup renderedCluster entityLayouts
        ),

        ("relationsBetween", JArray $ do
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
        )
    ]

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

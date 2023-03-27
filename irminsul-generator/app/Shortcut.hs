module Shortcut where

import Irminsul
import Data.List (nub)
import Utility.Vector

-- | Shortcut for (Entity, Information)
ip :: a -> String -> [String] -> Existence -> String -> (a, Information)
ip entity name aliases existence information =
    (entity, Information name aliases existence information)

-- Shortcuts for relation
ra :: String -> Entity -> Entity -> Relation
ra = Relation . Action
ba :: String -> Entity -> Entity -> Relation
ba = BiRelation . Action

-- | Shortcut of creating a Atom Character
ach :: String -> Entity
ach = (`Atom` Character)

-- | Shortcut of a root's title's layout
rl :: (Double, Double) -> (Double, Double) -> ShowcaseElementProperty
rl (x, y) (anchorX, anchorY) =
    ShowcaseElementProperty (Vector2 x y) (Vector2 anchorX anchorY) 0 0

-- | Shortcut of an atom's layout
al :: Entity -> (Double, Double) -> (Entity, ShowcaseElementProperty)
al entity (x, y) =
    (entity, ShowcaseElementProperty (Vector2 x y) (Vector2 x y) 0 0)

-- | Shortcut of a cluster's layout
cl :: Entity ->
    (Double, Double) -> (Double, Double) -> Double -> Double ->
    (Entity, ShowcaseElementProperty)
cl entity (x, y) (anchorX, anchorY) width height =
    (entity, ShowcaseElementProperty
        (Vector2 x y) (Vector2 anchorX anchorY) width height)

{- |
    Create a new cluster node, where all entities and relations from children
    are appended to this new node.
-}
clusterNode ::
        String      -- node name
    ->  ClusterType -- node type
    ->  [Entity]    -- entities
    ->  [Relation]  -- relations
    ->  [Entity]    -- child clusters
    -> Maybe Layout
    ->  Entity
clusterNode name clusterType pEntities pRelations children =
    Cluster name clusterType
        (IndexedSetFamily
            (pEntities ++ children)
            (pEntities ++ children ++ nub (concatMap (elements . entities) children)))
        (IndexedSetFamily
            pRelations
            (pRelations ++ nub (concatMap (elements . relations) children)))

{- |
    Create a new leaf node, which it has no children.
-}
clusterLeaf ::
        String      -- leaf name
    ->  ClusterType -- leaf type
    ->  [Entity]    -- entities
    ->  [Relation]  -- relations
    -> Maybe Layout
    ->  Entity
clusterLeaf name clusterType entities relations =
    Cluster name clusterType
        (IndexedSetFamily entities entities)
        (IndexedSetFamily relations relations)

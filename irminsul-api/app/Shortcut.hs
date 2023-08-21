module Shortcut where

import Irminsul
import Data.List (nub)
import Data.Vector

-- | Shortcut for (Entity, Information)
ip :: a -> String -> [String] -> String -> (a, Information)
ip entity name aliases information =
    (entity, Information name aliases information)

-- | Shortcut for (Entity, Information) but only name is provided
ipn :: a -> String -> (a, Information)
ipn entity name = (entity, Information name [] "")

-- Shortcuts for relation
ra :: String -> Entity -> Entity -> Relation
ra = Relation
ba :: String -> Entity -> Entity -> Relation
ba = BiRelation

-- | Shortcut of creating an Atom Character
-- ach :: String -> Entity
-- ach = (`Atom` Character)
-- | Shortcut of creating an Atom Object
ao :: String -> Entity
ao = (`Atom` Object) 

-- | Shortcut of a root's title's layout
rl :: (Double, Double) -> ShowcaseElementProperty
rl (x, y) = ShowcaseElementProperty (Vector2 x y) (Vector2 x y) (Vector2 0 0)

-- | Shortcut of an atom's layout
al :: Entity -> (Double, Double) -> (Entity, ShowcaseElementProperty)
al entity (x, y) =
    (entity, ShowcaseElementProperty (Vector2 x y) (Vector2 x y) (Vector2 0 0))

-- | Shortcut of a cluster's layout
cl :: Entity ->
    (Double, Double) -> (Double, Double) -> (Double, Double) ->
    (Entity, ShowcaseElementProperty)
cl entity (x, y) (anchorX, anchorY) (width, height) =
    (entity, ShowcaseElementProperty
        (Vector2 x y) (Vector2 anchorX anchorY) (Vector2 width height))

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
    -> Maybe RelationGraphLayout
    ->  Entity
clusterNode name clusterType pEntities pRelations children =
    Cluster name clusterType
        (IndexedSetFamily
            (pEntities ++ children)
            (nub (pEntities ++ children ++ concatMap (elements . entities) children)))
        (IndexedSetFamily
            pRelations
            (nub (pRelations ++ concatMap (elements . relations) children)))

{- |
    Create a new leaf node, which it has no children.
-}
clusterLeaf ::
        String      -- leaf name
    ->  ClusterType -- leaf type
    ->  [Entity]    -- entities
    ->  [Relation]  -- relations
    -> Maybe RelationGraphLayout
    ->  Entity
clusterLeaf name clusterType entities relations =
    Cluster name clusterType
        (IndexedSetFamily entities entities)
        (IndexedSetFamily relations relations)

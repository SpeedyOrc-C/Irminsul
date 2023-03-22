module Shortcut where

import Irminsul
import Data.List (nub)

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
    ->  Entity
clusterLeaf name clusterType entities relations =
    Cluster name clusterType
        (IndexedSetFamily entities entities)
        (IndexedSetFamily relations relations)

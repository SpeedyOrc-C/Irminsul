module Irminsul where

import Milestone
import Data.List (nub)

data Language
    = ZhCn
    | EnUs
    deriving (Eq, Show)

class Unique object => Translatable object where
    translate :: Language -> object -> Information


class Unique a where
    uniqueId :: a -> String

instance Unique Entity where
    uniqueId :: Entity -> String
    uniqueId (Atom id t) = show t ++ "-" ++ id
    uniqueId (Cluster id t _ _) = show t ++ "-" ++ id

instance Eq Entity where
    (==) :: Entity -> Entity -> Bool
    (Atom id1 type1) == (Atom id2 type2) = id1 == id2 && type1 == type2
    (Cluster id1 type1 _ _) == (Cluster id2 type2 _ _) = id1 == id2 && type1 == type2
    _ == _ = False

instance Unique Action where
    uniqueId :: Action -> String
    uniqueId (Action id) = id

instance Unique Relation where
    uniqueId :: Relation -> String
    uniqueId (Relation action from to) =
        show action ++ "_"
        ++ show from ++ "."
        ++ show to


newtype Action = Action String deriving (Eq)

instance Show Action where
    show :: Action -> String
    show (Action id) = id


data Relation =
    Relation Action Entity Entity
    deriving Eq

instance Show Relation where
    show :: Relation -> String
    show (Relation action from to) =
        show action ++ "("
        ++ show from ++ ", "
        ++ show to ++ ")"


data Time
    = YearsAgo Integer
    | Timestamp Milestone
    deriving Show

data Existence
    = Always
    | Before Time
    | After Time
    | Time `Between` Time
    | UntilNow
    | Unknown
    deriving Show

data Alias = Alias {
        alias :: String,
        explanation :: String
    } deriving Show

data Information = Information {
        name :: String,
        aliases :: [Alias],
        existence :: Existence,
        detail :: String
    } deriving Show

data AtomType
    = Character
    | Object
    deriving (Eq, Show)

data ClusterType
    = Root
    | World
    | Country
    | Organization
    | Property
    deriving (Eq, Show)

data IndexedSetFamily a = IndexedSetFamily {
    indices :: [a],
    elements :: [a]
}

instance Show a => Show (IndexedSetFamily a) where
  show :: Show a => IndexedSetFamily a -> String
  show = show . indices


data Entity
    = Atom {
        entityIdentifier :: String,
        atomType :: AtomType
    }
    | Cluster {
        entityIdentifier :: String,
        clusterType :: ClusterType,
        entities :: IndexedSetFamily Entity,
        relations :: IndexedSetFamily Relation
    }

instance Show Entity where
  show :: Entity -> String
  show atom@(Atom {}) = uniqueId atom
  show cluster@(Cluster _ _ entities relations) =
    uniqueId cluster ++ " "
    ++ show entities ++ " "
    ++ show relations


newtype ClusterIndex = ClusterIndex [String] deriving Show

generateIndex :: [Entity] -> ClusterIndex
generateIndex = ClusterIndex . map entityIdentifier

clusterNode ::
        String      -- node name
    ->  ClusterType -- node type
    ->  [Entity]    -- entities
    ->  [Relation]  -- relations
    ->  [Entity]    -- child clusters
    ->  Entity
{-
    Create a new cluster node, where all entities and relations from children
    are appended to this new node.
-}
clusterNode name clusterType pEntities pRelations children =
    Cluster name clusterType
        (IndexedSetFamily
            (pEntities ++ children)
            (pEntities ++ children ++ nub (concatMap (elements . entities) children)))
        (IndexedSetFamily
            pRelations
            (pRelations ++ nub (concatMap (elements . relations) children)))


clusterLeaf ::
        String      -- leaf name
    ->  ClusterType -- leaf type
    ->  [Entity]    -- entities
    ->  [Relation]  -- relations
    ->  Entity
{-
    Create a new leaf node, which it has no children.
-}
clusterLeaf name clusterType entities relations =
    Cluster name clusterType
        (IndexedSetFamily entities entities)
        (IndexedSetFamily relations relations)

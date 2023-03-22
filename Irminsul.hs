module Irminsul where

import Milestone
import Data.List (nub)

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


data Relation
    -- | Unidirectional relation
    = Relation Action Entity Entity
    -- | Bidirectional relation
    | BiRelation Action Entity Entity
    deriving (Eq)

expandBiRelation :: [Relation] -> [Relation]
expandBiRelation relations = do
    relation <- relations
    p relation
    where
        p r@(Relation {}) = [r]
        p r@(BiRelation action a b) = [Relation action a b, Relation action b a]

instance Show Relation where
    show :: Relation -> String
    show (Relation action from to) =
        show from ++ " --" ++ show action ++ "-> " ++ show to
    show (BiRelation action a b) =
        show a ++ " <-" ++ show action ++ "-> " ++ show b

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
    deriving (Eq)

instance Show AtomType where
    show :: AtomType -> String
    show Character = "CHR"
    show Object = "OBJ"

data ClusterType
    = Root
    | World
    | Country
    | Island
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

-- | Shortcut of creating a Atom Character
ach = (`Atom` Character)

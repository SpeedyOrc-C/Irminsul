module Irminsul where

import Data.List (nub)

import Milestone

newtype Action = Action {actionId :: String} deriving (Eq)

instance Show Action where
    show :: Action -> String
    show (Action id) = id

data Relation
    -- | Unidirectional relation
    = Relation Action Entity Entity
    -- | Bidirectional relation
    | Birelation Action Entity Entity
    deriving (Eq)

instance Show Relation where
    show :: Relation -> String
    show (Relation action from to) =
        entityId from ++ " -" ++ show action ++ "→ " ++ entityId to
    show (Birelation action a b) =
        entityId a ++ " ←" ++ show action ++ "→ " ++ entityId b

expandBirelation :: [Relation] -> [Relation]
expandBirelation = concatMap p
    where
        p r@(Relation {}) = [r]
        p (Birelation action a b) = [Relation action a b, Relation action b a]

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
        aliases :: [String],
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
    | Area
    | Organization
    | AnotherMe
    | Property
    deriving (Eq, Show)

data IndexedSetFamily a = IndexedSetFamily {
    indices :: [a],
    elements :: [a]
}

instance Show a => Show (IndexedSetFamily a) where
  show :: Show a => IndexedSetFamily a -> String
  show = show . indices

{- |
    An entity could be an Atom or a Cluster.
    Atom cannot be split further.
    Cluster consists of many Atoms and Clusters, which means Cluster can form
    a tree-like structure. It also consists of all relations about its entities.
-}
data Entity
    = Atom {
        entityId :: String,
        atomType :: AtomType
    }
    | Cluster {
        entityId :: String,
        clusterType :: ClusterType,
        entities :: IndexedSetFamily Entity,
        relations :: IndexedSetFamily Relation
    }

instance Eq Entity where
    (==) :: Entity -> Entity -> Bool
    (Atom id1 type1) == (Atom id2 type2) =
        id1 == id2 && type1 == type2
    (Cluster id1 type1 _ _) == (Cluster id2 type2 _ _) =
        id1 == id2 && type1 == type2
    _ == _ = False

instance Show Entity where
  show :: Entity -> String
  show atom@(Atom {}) = entityId atom
  show cluster@(Cluster _ _ entities relations) =
    entityId cluster ++ " "
    ++ show entities ++ " "
    ++ show relations

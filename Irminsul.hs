module Irminsul where

import Milestone

data Language
    = ZhCn
    | EnUs

class Unique object => Translatable object where
    translate :: Language -> object -> Information


class Unique a where
    uniqueId :: a -> String

instance Unique Entity where
    uniqueId :: Entity -> String
    uniqueId (Atom id t) = show t ++ "-" ++ id
    uniqueId (Cluster id t _ _) = show t ++ "-" ++ id

instance Unique Action where
    uniqueId :: Action -> String
    uniqueId = actionIdentifier

instance Unique Relation where
    uniqueId (Relation action from to) =
        show action ++ "_"
        ++ show from ++ "."
        ++ show to


newtype Action = Action {
        actionIdentifier :: String
    }

instance Show Action where
    show = actionIdentifier


data Relation =
    Relation Action Entity Entity

instance Show Relation where
    show (Relation action from to) =
        show action ++ "("
        ++ show from ++ ", "
        ++ show to ++ ")"

mother = Relation $ Action "Mother"
father = Relation $ Action "Father"
child = Relation $ Action "Child"
youngerSister = Relation $ Action "YoungerSister"
elderSister = Relation $ Action "ElderSister"


data Time
    = YearsAgo Integer
    | Timestamp Milestone
    deriving Show

data Existence
    = Before Time
    | UntilNow
    | After Time
    | Time `Between` Time
    | Always
    | Unknown
    deriving Show

data Information = Information {
        name :: String,
        existence :: Existence,
        introduction :: String
    } deriving Show

data AtomType
    = Character
    | Object
    deriving Show

data ClusterType
    = Country
    | Organization
    | Property
    deriving Show

data Entity
    = Atom {
        entityIdentifier :: String,
        atomType :: AtomType
    }
    | Cluster {
        entityIdentifier :: String,
        clusterType :: ClusterType,
        entities :: [Entity],
        relations :: [Relation]
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

{-
Merge a parent cluster's children into their parent.
Only merge entities and relations,
identifier and type still follows parent's.
-}
mergeFromChildClusters :: Entity -> [Entity] -> Entity
mergeFromChildClusters
    (Cluster parentIdentifier parentType parentEntities parentRelations)
    children
    = Cluster parentIdentifier parentType
        (concat $ parentEntities : map entities children)
        (concat $ parentRelations : map relations children)

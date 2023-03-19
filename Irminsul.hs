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
    uniqueId = entityIdentifier

instance Unique Action where
    uniqueId :: Action -> String
    uniqueId = actionIdentifier

newtype Action = Action {
        actionIdentifier :: String
    } deriving Show

data Relation = Relation Action Entity Entity deriving Show

mother = Relation $ Action "Action_Mother"
father = Relation $ Action "Action_Father"
child = Relation $ Action "Action_Child"

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

data Entity
    = Atom {
        entityIdentifier :: String
    }
    | Cluster {
        entityIdentifier :: String,
        entities :: [Entity],
        relations :: [Relation]
    } deriving Show

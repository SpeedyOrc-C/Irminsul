{-# LANGUAGE OverloadedStrings #-}

module Irminsul where

import Milestone
import Data.List (intercalate)
import Data.Vector
import Data.String


newtype Path = Path [Entity] deriving (Eq, Ord)

instance Semigroup Path where
    (Path path1) <> (Path path2) = Path $ path1 <> path2

depth :: Path -> Int
depth (Path path) = length path

instance Show Path where
    show (Path path) = intercalate " > " (map entityId path)

newtype Action = Action {actionId :: String} deriving (Eq)

instance Show Action where
    show (Action id) = id

data Relation
    = Relation {action::Action, subject::Entity, object:: Entity}
    | BiRelation {action::Action, subject::Entity, object:: Entity}

instance Eq Relation where
    (Relation a1 s1 o1) == (Relation a2 s2 o2) =
        a1 == a2 && s1 == s2 && o1 == o2
    (BiRelation a1 s1 o1) == (BiRelation a2 s2 o2) =
        a1 == a2 && (s1 == s2 && o1 == o2 || s1 == o2 && s2 == o1)
    _ == _ = False

instance Show Relation where
    show (Relation action from to) =
        entityId from ++ " -" ++ show action ++ "→ " ++ entityId to
    show (BiRelation action a b) =
        entityId a ++ " ←" ++ show action ++ "→ " ++ entityId b

isRelation :: Relation -> Bool
isRelation (Relation {}) = True
isRelation _ = False

isBiRelation :: Relation -> Bool
isBiRelation (BiRelation {}) = True
isBiRelation _ = False

matchSubject :: Entity -> Relation -> Bool
matchSubject entity (Relation _ subject _) = entity == subject
matchSubject _ _ = False

matchObject :: Entity -> Relation -> Bool
matchObject entity (Relation _ _ object) = entity == object
matchObject _ _ = False

swapBiRelationSubject :: Entity -> Relation -> Relation
swapBiRelationSubject subject r@(BiRelation action rSubject rObject) =
    if subject == rSubject then r else
    BiRelation action rObject rSubject

expandBiRelation :: [Relation] -> [Relation]
expandBiRelation = concatMap p
    where
        p r@(Relation {}) = [r]
        p (BiRelation action a b) = [Relation action a b, Relation action b a]

sameEntityPair :: Relation -> Relation -> Bool
sameEntityPair (Relation _ a b) (Relation _ c d) = sameEntityPair' a b c d
sameEntityPair (BiRelation _ a b) (BiRelation _ c d) = sameEntityPair' a b c d
sameEntityPair (BiRelation _ a b) (Relation _ c d) = sameEntityPair' a b c d
sameEntityPair (Relation _ a b) (BiRelation _ c d) = sameEntityPair' a b c d
sameEntityPair' :: Entity -> Entity -> Entity -> Entity -> Bool
sameEntityPair' a b c d = a == c && b == d || a == d && b == c

subjectAndObject :: Relation -> (Entity, Entity)
subjectAndObject (Relation _ a b) = (a, b)
subjectAndObject (BiRelation _ a b) = (a, b)

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
  show = show . indices

data RelationGraphLayout = RelationGraphLayout {
    rootProperty :: ShowcaseElementProperty,
    elementProperties :: [(Entity, ShowcaseElementProperty)]
}

data ShowcaseElementProperty = ShowcaseElementProperty {
    position :: Vector2,
    anchor :: Vector2,
    size :: Vector2
} deriving Show

elemLayout :: Entity -> RelationGraphLayout -> Bool
entity `elemLayout` (RelationGraphLayout _ entities) =
    entity `elem` (fst <$> entities)

{- |
    An entity could be an Atom or a Cluster.
    Atom cannot be split further.
    Cluster consists of many Atoms and Clusters, which means Cluster can form
    a tree-like structure. It also consists of all relations about its entities.
-}

instance IsString Entity where
    fromString id = Atom id Character

data Entity
    = Atom {
        entityId :: String,
        atomType :: AtomType
    }
    | Cluster {
        entityId :: String,
        clusterType :: ClusterType,
        entities :: IndexedSetFamily Entity,
        relations :: IndexedSetFamily Relation,
        layout :: Maybe RelationGraphLayout
    }

isCluster (Cluster {}) = True
isCluster _ = False

isAtom (Atom {}) = True
isAtom _ = False

instance Eq Entity where
    (Atom id1 type1) == (Atom id2 type2) =
        id1 == id2 && type1 == type2
    (Cluster id1 type1 _ _ _) == (Cluster id2 type2 _ _ _) =
        id1 == id2 && type1 == type2
    _ == _ = False

instance Ord Entity where
    x <= y = entityId x <= entityId y

instance Show Entity where
  show atom@(Atom {}) = entityId atom
  show cluster@(Cluster _ _ entities relations _) =
    entityId cluster ++ " "
    ++ show entities ++ " "
    ++ show relations

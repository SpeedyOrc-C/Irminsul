{-# LANGUAGE OverloadedStrings #-}

module Irminsul where

import Data.List (intercalate)
import Data.Vector ( Vector2 )
import Data.String ( IsString(..) )
import Prelude hiding (id)


newtype Path = Path [Entity] deriving (Eq, Ord)

instance Semigroup Path where
    (<>) :: Path -> Path -> Path
    (Path path1) <> (Path path2) = Path $ path1 <> path2

depth :: Path -> Int
depth (Path path) = length path

instance Show Path where
    show :: Path -> String
    show (Path path) = intercalate " > " (map entityId path)


data Relation
    = Relation   { action :: String, subject :: Entity, object :: Entity}
    | BiRelation { action :: String, subject :: Entity, object :: Entity}

instance Eq Relation where
    (==) :: Relation -> Relation -> Bool
    (Relation a1 s1 o1) == (Relation a2 s2 o2) =
            a1 == a2 && s1 == s2 && o1 == o2
    (BiRelation a1 s1 o1) == (BiRelation a2 s2 o2) =
        a1 == a2 && (s1 == s2 && o1 == o2 || s1 == o2 && s2 == o1)
    _ == _ = False

instance Show Relation where
    show :: Relation -> String
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
swapBiRelationSubject _ _ = error "Only BiRelation is allowed."

expandBiRelationAsRelation :: [Relation] -> [Relation]
expandBiRelationAsRelation = concatMap expand
    where
        expand r@(Relation {}) = return r
        expand (BiRelation action a b) =
            [Relation action a b, Relation action b a]

sameEntityPair :: Relation -> Relation -> Bool
sameEntityPair r1 r2 = s1 == s2 && o1 == o2 || s1 == o2 && s2 == o1
    where 
        s1 = subject r1; o1 = object r1
        s2 = subject r2; o2 = object r2

subjectAndObject :: Relation -> (Entity, Entity)
subjectAndObject relation = (subject relation, object relation)

data Alias = Alias {
        alias :: String,
        explanation :: String
    } deriving Show

data Information = 
    Information { name :: String, aliases :: [String], detail :: String }
    deriving Show

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
    | Family
    | AnotherMe
    | Property
    deriving (Eq, Show)

data IndexedSetFamily a = IndexedSetFamily { indices :: [a], elements :: [a] }

instance Show a => Show (IndexedSetFamily a) where
  show :: Show a => IndexedSetFamily a -> String
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
    Cluster consists of many Atoms and Clusters,
    which means Cluster can form a tree-like structure.
    It also consists of all relations about its entities.
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
        relations :: IndexedSetFamily Relation,
        layout :: Maybe RelationGraphLayout
    }

instance IsString Entity where
    fromString :: String -> Entity
    fromString id = Atom id Character

instance Eq Entity where
    (==) :: Entity -> Entity -> Bool
    e1 == e2 = entityId e1 == entityId e2

instance Ord Entity where
    (<=) :: Entity -> Entity -> Bool
    x <= y = entityId x <= entityId y

instance Show Entity where
    show :: Entity -> String
    show atom@(Atom {}) = entityId atom
    show cluster@(Cluster _ _ entities relations _) =
        entityId cluster ++ " "
        ++ show entities ++ " "
        ++ show relations

isCluster :: Entity -> Bool
isCluster (Cluster {}) = True
isCluster _ = False

isAtom :: Entity -> Bool
isAtom (Atom {}) = True
isAtom _ = False

module LanguagePack where

import Data.Maybe ( fromMaybe )

import Irminsul

data Language
    = ZhCn
    | EnUs
    deriving (Eq, Show)

data LanguagePack = LanguagePack {
    language :: Language,
    translationEntity :: [(Entity, Information)],
    translationAction :: [(Action, String)]
    }

translateEntity :: LanguagePack -> Entity -> String
translateEntity (LanguagePack _ translationEntities _) entity =
    maybe ("[" ++ entityId entity ++ "]")
        name (lookup entity translationEntities)

translateRelation :: LanguagePack -> Relation -> String
translateRelation languagePack (Relation action from to) =
    translateEntity languagePack from
    ++ " -" ++ translateAction languagePack action ++ "→ "
    ++ translateEntity languagePack to

translateRelation languagePack (Birelation action from to) =
    translateEntity languagePack from
    ++ " ←" ++ translateAction languagePack action ++ "→ "
    ++ translateEntity languagePack to

translateAction :: LanguagePack -> Action -> String
translateAction (LanguagePack _ _ translationActions) action =
    fromMaybe ("[" ++ actionId action ++ "]")
        (lookup action translationActions)

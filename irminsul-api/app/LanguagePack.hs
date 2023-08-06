module LanguagePack where

import Data.Maybe ( fromMaybe )

import Irminsul

data Language = ZhCn | EnUs
    deriving (Eq, Show)

readLanguageCode :: String -> Maybe Language
readLanguageCode lang = lookup lang [
    ("zh-cn", ZhCn),
    ("en-us", EnUs)]

data LanguagePack = LanguagePack {
    language :: Language,
    translationEntity :: [(Entity, Information)],
    translationAction :: [(Action, String)]
    }

translateEntityWith :: LanguagePack -> Entity -> String
translateEntityWith (LanguagePack _ translationEntities _) entity =
    maybe ("Entity-" ++ entityId entity)
        name (lookup entity translationEntities)

translateActionWith :: LanguagePack -> Action -> String
translateActionWith (LanguagePack _ _ translationActions) action =
    fromMaybe (actionId action)
        (lookup action translationActions)

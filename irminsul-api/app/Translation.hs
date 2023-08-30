{-# LANGUAGE LambdaCase #-}

module Translation where

import Data.Maybe ( fromMaybe )
import Data.Char (toLower)

import Irminsul
import Translation.ZhCn.Entity (translationEntityZhCn)
import Translation.ZhCn.Action (translationActionZhCn)
import Translation.EnUs.Entity (translationEntityEnUs)
import Translation.EnUs.Action (translationActionEnUs)

data Language = ZhCn | EnUs
    deriving (Eq, Show)

readLanguageCode :: String -> Maybe Language
readLanguageCode lang = lookup (toLower <$> lang) [
    ("zh", ZhCn),
    ("zh-tw", ZhCn),
    ("zh-cn", ZhCn),
    ("en", EnUs),
    ("en-us", EnUs),
    ("en-gb", EnUs)
    ]

data LanguagePack = LanguagePack {
    language :: Language,
    translationEntity :: [(String, Information)],
    translationAction :: [(String, String)]
    }

getLanguagePack :: Language -> LanguagePack
getLanguagePack = \case {
    ZhCn -> LanguagePack ZhCn translationEntityZhCn translationActionZhCn;
    EnUs -> LanguagePack EnUs translationEntityEnUs translationActionEnUs;
}

translateEntityWith :: LanguagePack -> Entity -> String
translateEntityWith (LanguagePack _ translationEntities _) entity =
    let eid = entityId entity in
    maybe ("Entity-" ++ eid) name (lookup eid translationEntities)

translateActionWith :: LanguagePack -> String -> String
translateActionWith (LanguagePack _ _ translationActions) action =
    fromMaybe ("Action-" ++ action) (lookup action translationActions)

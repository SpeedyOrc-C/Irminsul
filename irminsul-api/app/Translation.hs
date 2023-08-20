{-# LANGUAGE LambdaCase #-}

module Translation where

import Data.Maybe ( fromMaybe )

import Irminsul
import Translation.ZhCn.Entity (translationEntityZhCn)
import Translation.ZhCn.Action (translationActionZhCn)
import Translation.EnUs.Entity (translationEntityEnUs)
import Translation.EnUs.Action (translationActionEnUs)

data Language = ZhCn | EnUs
    deriving (Eq, Show)

readLanguageCode :: String -> Maybe Language
readLanguageCode lang = lookup lang [
    ("zh-cn", ZhCn),
    ("en-us", EnUs)]

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

translateActionWith :: LanguagePack -> Action -> String
translateActionWith (LanguagePack _ _ translationActions) action =
    fromMaybe ("Action-" ++ actionId action)
        (lookup (actionId action) translationActions)

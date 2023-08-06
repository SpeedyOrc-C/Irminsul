{-# LANGUAGE LambdaCase #-}

module Translation where

import LanguagePack
import Translation.ZhCn (languagePackZhCn)
import Translation.EnUs (languagePackEnUs)

getLanguagePack :: Language -> LanguagePack
getLanguagePack = \case {
    ZhCn -> languagePackZhCn;
    EnUs -> languagePackEnUs;
}

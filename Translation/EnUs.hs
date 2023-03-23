module Translation.EnUs where

import LanguagePack
import Translation.EnUs.Entity (translationEntityEnUs)
import Translation.EnUs.Action (translationActionEnUs)

languagePackEnUs = LanguagePack EnUs
    translationEntityEnUs translationActionEnUs

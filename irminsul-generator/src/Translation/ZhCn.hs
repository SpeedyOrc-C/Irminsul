module Translation.ZhCn where

import LanguagePack
import Translation.ZhCn.Entity (translationEntityZhCn)
import Translation.ZhCn.Action (translationActionZhCn)

languagePackZhCn = LanguagePack ZhCn
    translationEntityZhCn translationActionZhCn

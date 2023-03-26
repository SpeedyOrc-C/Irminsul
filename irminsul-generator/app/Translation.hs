module Translation where

import LanguagePack
import Translation.ZhCn (languagePackZhCn)
import Translation.EnUs (languagePackEnUs)

getLanguagePack :: Language -> LanguagePack
getLanguagePack ZhCn = languagePackZhCn
getLanguagePack EnUs = languagePackEnUs

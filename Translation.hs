module Translation where

import LanguagePack
import Translation.ZhCn (languagePackZhCn)
import Translation.EnUs (languagePackEnUs)

getPack ZhCn = languagePackZhCn
getPack EnUs = languagePackEnUs

module Translation.ZhCn.Entity where

import Irminsul
import Translation

import Root.AnotherWorld

instance Translatable Entity where
    translate :: Language -> Entity -> Maybe Information
    translate ZhCn e = lookup e [
        ip aether
            "空"
            []
            UntilNow
            "",
        ip lumine
            "荧"
            []
            UntilNow
            ""
        ]

module Translation.ZhCn where

import Irminsul
import Root

instance Translatable Entity where
    translate :: Language -> Entity -> Information
    translate ZhCn entity = case uniqueId entity of {
        "Character-Aether" -> Information "空" UntilNow "";
        "Character-Lumine" -> Information "荧" UntilNow "";
        -- _ -> error $ uniqueId entity ++ "\" not found.";
    }

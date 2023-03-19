module Translation.ZhCn where

import Irminsul

instance Translatable Entity where
    translate :: Language -> Entity -> Information
    translate ZhCn entity = let id = uniqueId entity in case id of {
        "Character_Aether" -> Information "空" UntilNow "";
        "Character_Lumine" -> Information "荧" UntilNow "";
        _ -> error $ "Identifier \"" ++ id ++ "\" not found.";
    }

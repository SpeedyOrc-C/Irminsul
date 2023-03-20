module Translation.ZhCn where

import Irminsul
import Root

instance Translatable Entity where
    translate :: Language -> Entity -> Information
    translate ZhCn entity = translatorZhCn (uniqueId entity)

translatorZhCn :: String -> Information
translatorZhCn id = case id of {
    "Character-Aether" -> Information "空" [] UntilNow "";
    "Character-Lumine" -> Information "荧" [] UntilNow "";
    _ -> error $ "[" ++ id ++ "] has not yet translated.";   
}

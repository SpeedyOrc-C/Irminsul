module Translation.ZhCn.Entity where

import Irminsul
import Translation

instance Translatable Relation where
    translate :: Language -> Relation -> Maybe Information
    translate ZhCn = translatorRelationZhCn


translatorRelationZhCn :: Relation -> Maybe Information
translatorRelationZhCn r
    | otherwise = Nothing

module Translation.ZhCn.Relation where

import Irminsul
import Translation

import Root.AnotherWorld

instance Translatable Relation where
    translate :: Language -> Relation -> Maybe Information
    translate ZhCn = translatorRelationZhCn


translatorRelationZhCn :: Relation -> Maybe Information
translatorRelationZhCn r
    | otherwise = Nothing

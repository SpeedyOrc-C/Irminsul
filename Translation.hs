module Translation () where

import Data.Maybe (fromJust)

import Irminsul
import Translation.ZhCn.Entity (translationEntityZhCn)
import Translation.EnUs.Entity

data Language
    = ZhCn
    | EnUs
    deriving (Eq, Show)

class Unique object => Translatable object where
    translate :: Language -> object -> Maybe Information
    translateName :: Language -> object -> String
    notYetTranslated :: Language -> object -> String

instance Translatable Entity where
    notYetTranslated :: Language -> Entity -> String
    notYetTranslated language (Atom id _) =
        "[Atom " ++ id ++ " " ++ show language ++ "]"
    notYetTranslated language (Cluster id _ _ _) =
        "[Cluster " ++ id ++ " " ++ show language ++ "]"

    translateName :: Language -> Entity -> String
    translateName language e =
        maybe (notYetTranslated language e) name (translate language e)
    
    translate :: Language -> Entity -> Maybe Information
    translate language entity =
        lookup entity (fromJust $ lookup language languageMapper) where
            languageMapper = [
                    (ZhCn, translationEntityZhCn),
                    (EnUs, translationEntityEnUs)
                ]

instance Translatable Relation where
    translate :: Language -> Relation -> Maybe Information
    translate = undefined
    translateName :: Language -> Relation -> String
    translateName = undefined
    notYetTranslated :: Language -> Relation -> String
    notYetTranslated = undefined

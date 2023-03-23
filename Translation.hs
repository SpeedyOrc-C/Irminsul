module Translation () where

import Data.Maybe (fromJust)

import Irminsul
import Translation.ZhCn.Entity (translationEntityZhCn)
import Translation.EnUs.Entity

data Language
    = ZhCn
    | EnUs
    deriving (Eq, Show)

class Translatable a where
    translate :: Language -> a -> Maybe Information
    translateName :: Language -> a -> String
    notYetTranslated :: Language -> a -> String

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

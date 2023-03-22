module Translation where

import Irminsul

data Language
    = ZhCn
    | EnUs
    deriving (Eq, Show)

class Unique object => Translatable object where
    translate :: Language -> object -> Maybe Information

-- | Shortcut for (Entity, Information)
ip entity name aliases existance inforamtion =
    (entity, Information name aliases existance inforamtion)
module CommonRelations where

import Irminsul

-- Shortcuts
ra = Relation . Action
ba = BiRelation . Action

-- Unidirectional relations

mother = ra "Mother"
father = ra "Father"
son = ra "Son"
daughter = ra "Daughter"
youngerSister = ra "YoungerSister"
elderSister = ra "ElderSister"
youngerBrother = ra "YoungerBrother"
elderBrother = ra "ElderBrother"

liveIn = ra "LiveIn"
love = ra "Love"
create = ra "Create"
kill = ra "Kill"

-- Bidirectional relations

friend = ba "Friend"

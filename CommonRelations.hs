module CommonRelations where

import Irminsul

-- 1 way and 2 ways relation
ra = Relation . Action
ba = BiRelation . Action

-- Relations
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
friend = ba "Friend"
create = ra "Create"
kill = ra "Kill"

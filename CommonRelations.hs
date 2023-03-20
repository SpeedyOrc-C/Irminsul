module CommonRelations where

import Irminsul (Relation(Relation), Action (Action))

ra = Relation . Action

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

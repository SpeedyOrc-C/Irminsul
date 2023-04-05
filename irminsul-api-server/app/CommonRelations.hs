module CommonRelations where

import Irminsul
import Shortcut


-- Unidirectional relations

mother = ra "Mother"
father = ra "Father"
son = ra "Son"
daughter = ra "Daughter"
youngerSister = ra "YoungerSister"
elderSister = ra "ElderSister"
youngerBrother = ra "YoungerBrother"
elderBrother = ra "ElderBrother"

familiar = ra "Familiar"
youngerGeneration = ra "YoungerGeneration"
olderGeneration = ra "OlderGeneration"

student = ra "Student"
teacher = ra "Teacher"
guardian = ra "Guardian"
teamCaptain = ra "TeamCaptain"
actingGrandMaster = ra "ActingGrandMaster"

liveIn = ra "LiveIn"
love = ra "Love"
admire = ra "Admire"
create = ra "Create"
kill = ra "Kill"

foster relation x y = Relation (Action ("Foster" ++ id)) x y where
    (Relation (Action id) _ _) = relation x y

acting relation x y = Relation (Action ("Acting" ++ id)) x y where
    (Relation (Action id) _ _) = relation x y

samsara = ra "Samsara"

-- Bidirectional relations

friend = ba "Friend"
partner = ba "Partner"
enemy = ba "enemy"
colleague = ba "Colleague"
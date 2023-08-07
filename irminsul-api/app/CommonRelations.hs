module CommonRelations where

import Irminsul
import Shortcut


-- Unidirectional relations

rule = ra "Rule"

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
mentor = ra "Mentor"
guardian = ra "Guardian"
teamCaptain = ra "TeamCaptain"
grandMaster = ra "GrandMaster"
actingGrandMaster = ra "ActingGrandMaster"
attendant = ra "Attendant"
translator = ra "Translator"
pet = ra "Pet"

liveIn = ra "LiveIn"
love = ra "Love"
admire = ra "Admire"
appreciate = ra "Appreciate"
like = ra "Like"
create = ra "Create"
hate = ra "Hate"
lookAfter = ra "LookAfter"
dislike = ra "Dislike"
kill = ra "Kill"

superior = ra "Superior"

foster relation x y = Relation (Action ("Foster" ++ id)) x y where
    (Relation (Action id) _ _) = relation x y

acting relation x y = Relation (Action ("Acting" ++ id)) x y where
    (Relation (Action id) _ _) = relation x y

samsara = ra "Samsara"
tavernOwner = ra "TavernOwner"

allergicTo = ra "AllergicTo"

-- Bidirectional relations

friend = ba "Friend"
partner = ba "Partner"
enemy = ba "Enemy"
colleague = ba "Colleague"
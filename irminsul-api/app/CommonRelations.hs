{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CommonRelations where

import Irminsul



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
disciple = ra "Disciple"
assistant = ra "Assistant"
teacher = ra "Teacher"
mentor = ra "Mentor"
guardian = ra "Guardian"
teamCaptain = ra "TeamCaptain"
seaCaptain = ra "SeaCaptain"
grandMaster = ra "GrandMaster"
actingGrandMaster = ra "ActingGrandMaster"
attendant = ra "Attendant"
longTimeAudience = ra "LongTimeAudience"
translator = ra "Translator"
cook = ra "Cook"
customer = ra "Customer"
intelligenceOfficer = ra "IntelligenceOfficer"
pet = ra "Pet"

liveIn = ra "LiveIn"
love = ra "Love"
admire = ra "Admire"
appreciate = ra "Appreciate"
esteem = ra "Esteem"
like = ra "Like"
intrestedIn = ra "IntrestedIn"
create = ra "Create"
hate = ra "Hate"
lookAfter = ra "LookAfter"
dislike = ra "Dislike"
notSatisfiedWith = ra "NotSatisfiedWith"
kill = ra "Kill"
afraidOf = ra "AfraidOf"
bewareOf = ra "BewareOf"

superior = ra "Superior"

foster relation x y = Relation ("Foster" ++ id) x y where
    (Relation id _ _) = relation x y

acting relation x y = Relation ("Acting" ++ id) x y where
    (Relation id _ _) = relation x y

samsara = ra "Samsara"
tavernOwner = ra "TavernOwner"

allergicTo = ra "AllergicTo"

-- Bidirectional relations

friend = ba "Friend"
partner = ba "Partner"
enemy = ba "Enemy"
colleague = ba "Colleague"
cooperate = ba "Cooperate"
rival = ba "Rival"

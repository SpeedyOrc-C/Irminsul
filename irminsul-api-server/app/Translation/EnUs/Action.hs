module Translation.EnUs.Action where

import Control.Arrow (Arrow(first))

import Irminsul

translationActionEnUs = first Action <$> [
    ("ActingGrandMaster", "Acting grand master"),
    ("ActingGuardian", "Acting guardian"),
    ("Admire", "Admire"),
    ("AllergicTo", "Allergic to"),
    ("Attendant", "Attendant"),
    ("Create", "Create"),
    ("Daughter", "Daughter"),
    ("Dislike", "Dislike"),
    ("ElderBrother", "Elder brother"),
    ("ElderSister", "Elder sister"),
    ("Familiar", "Familiar"),
    ("Father", "Father"),
    ("FosterElderBrother", "Foster elder brother"),
    ("FosterElderSister", "Foster elder sister"),
    ("FosterYoungerBrother", "Foster younger brother"),
    ("FosterYoungerSister", "Foster younger sister"),
    ("Friend", "Friend"),
    ("GrandMaster", "Grand master"),
    ("Guardian", "Guardian"),
    ("Hate", "Hate"),
    ("Kill", "Kill"),
    ("Like", "Like"),
    ("LiveIn", "Live in"),
    ("Love", "Love"),
    ("Mother", "Mother"),
    ("Partner", "Partner"),
    ("Pet", "Pet"),
    ("Rule", "Rule"),
    ("Son", "Son"),
    ("Student", "Student"),
    ("TavernOwner", "Tavern owner"),
    ("SupportInSilence", "Support in silence"),
    ("Teacher", "Teacher"),
    ("TeamCaptain", "Team captain"),
    ("Translator", "Translator"),
    ("YoungerBrother", "Younger brother"),
    ("YoungerGeneration", "Younger generation"),
    ("YoungerSister", "Younger sister")
    ]

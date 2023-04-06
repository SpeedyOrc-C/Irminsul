module Translation.EnUs.Action where

import Control.Arrow (Arrow(first))

import Irminsul

translationActionEnUs = first Action <$> [
    ("ActingGrandMaster", "Acting grandMaster"),
    ("ActingGuardian", "Acting guardian"),
    ("Admire", "Admire"),
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
    ("Rule", "Rule"),
    ("Son", "Son"),
    ("Student", "Student"),
    ("SupportInSilence", "Support in silence"),
    ("Teacher", "Teacher"),
    ("TeamCaptain", "Team captain"),
    ("YoungerBrother", "Younger brother"),
    ("YoungerGeneration", "Younger generation"),
    ("YoungerSister", "Younger sister")
    ]

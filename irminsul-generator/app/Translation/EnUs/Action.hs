module Translation.EnUs.Action where

import Control.Arrow (Arrow(first))

import Irminsul

translationActionEnUs = first Action <$> [
    ("Mother", "mother"),
    ("Father", "father"),
    ("Son", "son"),
    ("Daughter", "daughter"),

    ("YoungerSister", "younger sister"),
    ("ElderSister", "elder sister"),
    ("YoungerBrother", "younger brother"),
    ("ElderBrother", "elder brother"),
    ("FosterYoungerSister", "foster younger sister"),
    ("FosterElderSister", "foster elder sister"),
    ("FosterYoungerBrother", "foster younger brother"),
    ("FosterElderBrother", "foster elder brother"),
    
    ("Friend", "friend"),
    ("Student", "student"),
    ("Teacher", "teacher"),
    ("Guardian", "guardian"),
    ("ActingGuardian", "acting guardian"),
    ("ActingGrandMaster", "acting grand master"),
    ("TeamCaptain", "captain"),
    ("LiveIn", "live in"),
    ("Love", "love"),
    ("Create", "create"),
    ("Kill", "kill")
    ]

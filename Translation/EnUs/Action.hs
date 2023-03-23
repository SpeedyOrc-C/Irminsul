module Translation.EnUs.Action where

import Control.Arrow (Arrow(first))

import Irminsul

translationActionEnUs = first Action <$> [
    ("Mother", "mother"),
    ("Father", "father"),
    ("Son", "son"),
    ("Daughter", "daughter"),
    ("YoungerSister", "younger Sister"),
    ("ElderSister", "elder sister"),
    ("YoungerBrother", "younger brother"),
    ("ElderBrother", "elder brother"),
    ("Student", "student"),
    ("Teacher", "teacher"),
    ("Guardian", "guardian"),
    ("Captain", "captain"),
    ("LiveIn", "live in"),
    ("Love", "love"),
    ("Create", "create"),
    ("Kill", "kill")
    ]

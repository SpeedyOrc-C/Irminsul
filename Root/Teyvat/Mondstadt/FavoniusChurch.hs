module Root.Teyvat.Mondstadt.FavoniusChurch where

import Irminsul

barbaraGunnhildr = Atom "BarbaraGunnhildr" Character
rosaria = Atom "Rosaria" Character

-- TODO: Topmost relations goes here

thisAtoms = [
    barbaraGunnhildr,
    rosaria
    ]

thisRelations = [
    -- TODO: Put all topmost relations into this list
    ]

favoniusChurch = Cluster
    "FavoniusChurch" Organization thisAtoms thisRelations

index = generateIndex []

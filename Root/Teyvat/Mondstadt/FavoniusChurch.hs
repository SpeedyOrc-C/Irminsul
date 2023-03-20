module Root.Teyvat.Mondstadt.FavoniusChurch where

import Irminsul

barbaraGunnhildr = Atom "BarbaraGunnhildr" Character
rosaria = Atom "Rosaria" Character

favoniusChurch = clusterLeaf "FavoniusChurch"
    Organization
    [
        barbaraGunnhildr,
        rosaria
    ]
    [

    ]

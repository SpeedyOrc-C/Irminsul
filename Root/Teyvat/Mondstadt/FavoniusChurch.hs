module Root.Teyvat.Mondstadt.FavoniusChurch where

import Irminsul

barbaraGunnhildr = ach "BarbaraGunnhildr"
rosaria = ach "Rosaria"

favoniusChurch = clusterLeaf "FavoniusChurch"
    Organization
    [
        barbaraGunnhildr,
        rosaria
    ]
    [

    ]

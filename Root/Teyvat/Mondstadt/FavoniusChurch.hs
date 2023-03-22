module Root.Teyvat.Mondstadt.FavoniusChurch where

import Irminsul
import Shortcut

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

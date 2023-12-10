module Root.Teyvat.Mondstadt.FavoniusChurch where

import Irminsul
import Root.Teyvat.Mondstadt.GunnhildrPegg

rosaria = "Rosaria"

favoniusChurch = clusterLeaf "FavoniusChurch"
    Organization
    [
        barbaraPegg,
        rosaria,
        simonPegg
    ]
    [
        
    ]
    (layout (0, 15) [
        al simonPegg (-15, 0),
        al barbaraPegg (0, 0),
        al rosaria (15, 0)
    ])

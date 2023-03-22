module Root.Teyvat.Inazuma.WatatsumiIsland where

import Irminsul

kokomiSangonomiya = ach "KokomiSangonomiya"
gorou = ach "Gorou"

watatsumiIsland = clusterLeaf "WatatsumiIsland" Area
    [
        kokomiSangonomiya,
        gorou
    ]
    [
    ]

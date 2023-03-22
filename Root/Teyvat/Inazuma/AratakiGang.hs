module Root.Teyvat.Inazuma.AratakiGang where

import Irminsul

shinobuKuki = ach "ShinobuKuki"
ittoArataki = ach "IttoArataki"

aratakiGang = clusterLeaf "AratakiGang" Organization
    [
        shinobuKuki,
        ittoArataki
    ]
    [
    ]

module Root.Teyvat.Inazuma.AratakiGang where

import Irminsul
import Shortcut

shinobuKuki = ach "ShinobuKuki"
ittoArataki = ach "IttoArataki"

aratakiGang = clusterLeaf "AratakiGang" Organization
    [
        shinobuKuki,
        ittoArataki
    ]
    [
    ]

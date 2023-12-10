module Root.Teyvat.Mondstadt.TheFourWinds where

import Irminsul
import CommonRelations

venti = "Venti";
    barbatos = venti
dvalin = "Dvalin"
boreas = "Boreas"

theFourWinds = clusterLeaf "TheFourWinds" Organization
    [
        venti,
        dvalin,
        boreas
    ]
    [
        dvalin `familiar` venti,
        boreas `familiar` venti
    ]
    (layout (-15, 15) [
        al venti (0, 0),
        al dvalin (15, 0),
        al boreas (0, 15)
    ])

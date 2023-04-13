module Root.Teyvat.Mondstadt.TheFourWinds where

import Irminsul
import CommonRelations
import Shortcut

venti = ach "Venti";
    barbatos = venti
dvalin = ach "Dvalin"
boreas = ach "Boreas"

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
    (Just $ RelationGraphLayout {
        rootProperty = rl (-15, 15),
        elementProperties = [
            al venti (0, 0),
            al dvalin (15, 0),
            al boreas (0, 15)
        ]
    })

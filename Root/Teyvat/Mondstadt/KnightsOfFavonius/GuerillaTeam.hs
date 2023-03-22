module Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam where

import Irminsul
import Shortcut
import CommonRelations

eulaLaurence = ach "EulaLaurence"
mika = ach "Mika"

guerillaTeam = clusterLeaf "GuerillaTeam" Organization
    [
        eulaLaurence,
        mika
    ]
    [
        eulaLaurence `captain` guerillaTeam
    ]

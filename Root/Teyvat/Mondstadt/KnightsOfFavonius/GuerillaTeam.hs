module Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam where

import Irminsul
import Shortcut
import CommonRelations

eulaLaurence = ach "EulaLaurence"
mikaSchmidt = ach "MikaSchmidt"

guerillaTeam = clusterLeaf "GuerillaTeam" Organization
    [
        eulaLaurence,
        mikaSchmidt
    ]
    [
        eulaLaurence `captain` guerillaTeam
    ]

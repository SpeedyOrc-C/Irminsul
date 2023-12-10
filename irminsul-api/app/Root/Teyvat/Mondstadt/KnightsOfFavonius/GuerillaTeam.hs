module Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam where

import Irminsul
import CommonRelations

eulaLawrence = "EulaLawrence"
mikaSchmidt = "MikaSchmidt"

guerillaTeam = clusterLeaf "GuerillaTeam" Organization
    [
        eulaLawrence,
        mikaSchmidt
    ]
    [
        eulaLawrence `teamCaptain` guerillaTeam
    ]
    (layout (0, 15) [
        al eulaLawrence (-15, 0),
        al mikaSchmidt (15, 0)
    ])

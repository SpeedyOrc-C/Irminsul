module Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam where

import Irminsul
import Shortcut
import CommonRelations

eulaLawrence = ach "EulaLawrence"
mikaSchmidt = ach "MikaSchmidt"

guerillaTeam = clusterLeaf "GuerillaTeam" Organization
    [
        eulaLawrence,
        mikaSchmidt
    ]
    [
        eulaLawrence `teamCaptain` guerillaTeam
    ]
    (Just $ Layout [
        al eulaLawrence (-10, 0),
        al mikaSchmidt (10, 0)
    ])

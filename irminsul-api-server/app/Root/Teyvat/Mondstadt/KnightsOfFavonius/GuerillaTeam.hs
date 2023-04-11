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
    (Just $ RelationGraphLayout {
        rootProperty = rl (0, 15),
        elementProperties = [
            al eulaLawrence (-15, 0),
            al mikaSchmidt (15, 0)
        ]
    })

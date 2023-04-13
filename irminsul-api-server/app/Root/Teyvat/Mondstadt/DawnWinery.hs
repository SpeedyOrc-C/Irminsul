module Root.Teyvat.Mondstadt.DawnWinery where

import Irminsul
import Shortcut
import CommonRelations

dilucRagvindr = ach "DilucRagvindr"
kaeyaRagvindr = ach "KaeyaRagvindr"

dawnWinery = clusterLeaf "DawnWinery" Organization
    [
        dilucRagvindr,
        kaeyaRagvindr
    ]
    [
        foster youngerBrother kaeyaRagvindr dilucRagvindr,
        foster elderBrother dilucRagvindr kaeyaRagvindr
    ]
    (Just $ RelationGraphLayout {
        rootProperty = rl (0, 15),
        elementProperties = [
            al dilucRagvindr (-15, 0),
            al kaeyaRagvindr (15, 0)
        ]
    })

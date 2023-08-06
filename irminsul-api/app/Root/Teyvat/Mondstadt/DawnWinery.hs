{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Mondstadt.DawnWinery where

import Irminsul
import Shortcut
import CommonRelations

dilucRagvindr = "DilucRagvindr"
crepusRagvindr = "CrepusRagvindr"
kaeyaRagvindr = "KaeyaRagvindr"

dawnWinery = clusterLeaf "DawnWinery" Organization
    [
        dilucRagvindr,
        crepusRagvindr,
        kaeyaRagvindr
    ]
    [
        crepusRagvindr `father` dilucRagvindr,
        dilucRagvindr `son` crepusRagvindr,
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

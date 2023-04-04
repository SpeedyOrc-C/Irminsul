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
    Nothing

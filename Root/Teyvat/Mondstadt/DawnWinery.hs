module Root.Teyvat.Mondstadt.DawnWinery where

import Irminsul

dilucRagvindr = ach "DilucRagvindr"
kaeyaRagvindr = ach "KaeyaRagvindr"

dawnWinery = clusterLeaf "DawnWinery" Organization
    [
        dilucRagvindr,
        kaeyaRagvindr
    ]
    [

    ]

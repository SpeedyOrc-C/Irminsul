module Root.Teyvat.Mondstadt.DawnWinery where

import Irminsul
import Shortcut

dilucRagvindr = ach "DilucRagvindr"
kaeyaRagvindr = ach "KaeyaRagvindr"

dawnWinery = clusterLeaf "DawnWinery" Organization
    [
        dilucRagvindr,
        kaeyaRagvindr
    ]
    [

    ]

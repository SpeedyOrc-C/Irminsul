module Root.Teyvat.Kaeya where

import Irminsul

import Root.Teyvat.Khaenriah (kaeyaAlberich)
import Root.Teyvat.Mondstadt.DawnWinery (kaeyaRagvindr)

kaeya = clusterLeaf "Kaeya" AnotherMe
    [
        kaeyaAlberich,
        kaeyaRagvindr
    ]
    [
    ]

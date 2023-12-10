module Root.Teyvat.Liyue.Adepti.Yaksha where
-- 夜叉

import Irminsul

xiao = "Xiao";
    alatus = xiao
bosacius = "Bosacius"
indarias = "Indarias"
bonanus = "Bonanus"
menogias = "Menogias"
pervases = "Pervases"

yaksha = clusterLeaf "Yaksha" Organization
    [
        alatus,
        bosacius,
        indarias,
        bonanus,
        menogias,
        pervases
    ]
    [
    
    ]
    (layout (0, 0) [
        alp xiao (15, 0),
        alp bosacius (15, 60),
        alp indarias (15, 120),
        alp bonanus (15, 180),
        alp menogias (15, 240),
        alp pervases (15, 300)
    ])

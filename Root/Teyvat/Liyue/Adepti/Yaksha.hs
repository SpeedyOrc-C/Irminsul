module Root.Teyvat.Liyue.Adepti.Yaksha where
-- 夜叉

import Irminsul
import Shortcut

xiao = ach "Xiao";
    alatus = xiao
bosacius = ach "Bosacius"
indarias = ach "Indarias"
bonanus = ach "Bonanus"
menogias = ach "Menogias"
pervases = ach "Pervases"

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

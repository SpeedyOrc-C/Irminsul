module Root.Teyvat.Liyue.Adepti.Yaksha where
-- 夜叉

import Irminsul

xiao = Atom "Xiao" Character;
    alatus = xiao
bosacius = Atom "Bosacius" Character
indarias = Atom "Indarias" Character
bonanus = Atom "Bonanus" Character
menogias = Atom "Menogias" Character
pervases = Atom "Pervases" Character

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

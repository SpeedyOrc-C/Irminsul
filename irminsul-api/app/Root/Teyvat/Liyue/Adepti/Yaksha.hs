{-# LANGUAGE OverloadedStrings #-}

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
    Nothing

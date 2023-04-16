{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma where
-- 稲妻

import Irminsul
import Shortcut
import Root.Teyvat.Inazuma.TriCommission.YashiroCommission (yashiroCommission)
import Root.Teyvat.Inazuma.AratakiGang (aratakiGang)
import Root.Teyvat.Inazuma.TriCommission (triCommission)
import Root.Teyvat.Inazuma.WatatsumiIsland (watatsumiIsland)
import Root.Teyvat.Inazuma.Shuumatsuban (shuumatsuban)

eiRaiden = "EiRaiden";
    beelzebul = eiRaiden
raidenShogun = "RaidenShogun"
makotoRaiden = "MakotoRaiden";
    baal = makotoRaiden
kunikuzushiRaiden = "KunikuzushiRaiden";
    scaramouche = kunikuzushiRaiden

mikoYae = "MikoYae"
yoimiyaNaganohara = "YoimiyaNaganohara"

inazuma = clusterNode "Inazuma" Country
    [
        eiRaiden,
        raidenShogun,
        makotoRaiden,
        kunikuzushiRaiden,
        mikoYae,
        yoimiyaNaganohara
    ]
    [

    ]
    [
        triCommission,
        aratakiGang,
        watatsumiIsland,
        shuumatsuban
    ]
    Nothing

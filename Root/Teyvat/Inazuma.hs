{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Inazuma where
-- 稲妻

import Irminsul
import Shortcut
import Root.Teyvat.Inazuma.TriCommission.YashiroCommission (yashiroCommission)
import Root.Teyvat.Inazuma.AratakiGang (aratakiGang)
import Root.Teyvat.Inazuma.TriCommission (triCommission)
import Root.Teyvat.Inazuma.WatatsumiIsland (watatsumiIsland)
import Root.Teyvat.Inazuma.Shuumatsuban (shuumatsuban)

eiRaiden = ach "EiRaiden";
    beelzebul = eiRaiden
raidenShogun = ach "RaidenShogun"
makotoRaiden = ach "MakotoRaiden";
    baal = makotoRaiden
kunikuzushiRaiden = ach "KunikuzushiRaiden";
    wanderer = kunikuzushiRaiden;
    scaramouche = kunikuzushiRaiden

mikoYae = ach "MikoYae"
yoimiyaNaganohara = ach "YoimiyaNaganohara"

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

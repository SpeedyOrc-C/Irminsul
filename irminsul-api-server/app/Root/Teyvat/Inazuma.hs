{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma where
-- 稲妻

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Inazuma.TriCommission.YashiroCommission
import Root.Teyvat.Inazuma.AratakiGang
import Root.Teyvat.Inazuma.TriCommission
import Root.Teyvat.Inazuma.WatatsumiIsland
import Root.Teyvat.Inazuma.Shuumatsuban

eiRaiden = "EiRaiden";
    beelzebul = eiRaiden
raidenShogun = "RaidenShogun"
makotoRaiden = "MakotoRaiden";
    baal = makotoRaiden
kunikuzushiRaiden = "KunikuzushiRaiden";
    scaramouche = kunikuzushiRaiden

mikoYae = "MikoYae"
yoimiyaNaganohara = "YoimiyaNaganohara"

kaedeharaKazuha = "KaedeharaKazuha"
kirara = "Kirara"

inazuma = clusterNode "Inazuma" Country
    [
        eiRaiden,
        raidenShogun,
        makotoRaiden,
        kunikuzushiRaiden,
        mikoYae,
        yoimiyaNaganohara,
        kaedeharaKazuha,
        kirara
    ]
    [
        eiRaiden `rule` inazuma
    ]
    [
        triCommission,
        aratakiGang,
        watatsumiIsland,
        shuumatsuban
    ]
    Nothing

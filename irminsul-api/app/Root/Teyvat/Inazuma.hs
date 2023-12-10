module Root.Teyvat.Inazuma where
-- 稲妻

import Irminsul
import CommonRelations

import Root.Teyvat.Inazuma.Kaedehara
import Root.Teyvat.Inazuma.AratakiGang
import Root.Teyvat.Inazuma.TriCommission
import Root.Teyvat.Inazuma.WatatsumiIsland
import Root.Teyvat.Inazuma.Shuumatsuban
import Root.Teyvat.Inazuma.TriCommission.TenryouCommission

eiRaiden = "EiRaiden";
    beelzebul = eiRaiden
raidenShogun = "RaidenShogun"
makotoRaiden = "MakotoRaiden";
    baal = makotoRaiden
kunikuzushiRaiden = "KunikuzushiRaiden";
    scaramouche = kunikuzushiRaiden

mikoYae = "MikoYae"
yoimiyaNaganohara = "YoimiyaNaganohara"

kirara = "Kirara"

inazuma = clusterNode "Inazuma" Country
    [
        eiRaiden,
        raidenShogun,
        makotoRaiden,
        kunikuzushiRaiden,
        mikoYae,
        yoimiyaNaganohara,
        kirara
    ]
    [
        eiRaiden `rule` inazuma,
        mikoYae `familiar` eiRaiden,
        eiRaiden `create` kunikuzushiRaiden,
        eiRaiden `youngerSister` makotoRaiden,
        makotoRaiden `elderSister` eiRaiden
    ]
    [
        triCommission,
        aratakiGang,
        watatsumiIsland,
        shuumatsuban,
        kaedehara
    ]
    (layout (-15, 15) [
        al eiRaiden (-15, 0),
        al mikoYae (-30, 0),
        al makotoRaiden (-15, -15),
        al kunikuzushiRaiden (-30, -15),
        al kazuhaKaedehara (0, 0),
        al saraKujo (0, -15),

        cl kaedehara (7.5, 7.5) (7.5, 7.5) (12, 5),
        cl triCommission (0, -30) (0, -30) (15, 5)
    ])

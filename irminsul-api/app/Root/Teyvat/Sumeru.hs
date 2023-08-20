{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya
import Root.Teyvat.Sumeru.GrandharvaVille
import Root.Teyvat.Inazuma
import Root.Teyvat.Sumeru.Akademiya.Haravatat

rukkhadevataGreaterLord = "RukkhadevataGreaterLord"

nahida = "Nahida";
    buer = nahida

wanderer = "Wanderer"

dehya = "Dehya"
kusayla = "Kusayla"

sumeru = clusterNode "Sumeru" Country
    [
        nahida,
        rukkhadevataGreaterLord,
        wanderer,
        dehya,
        kusayla
    ]
    [
        rukkhadevataGreaterLord `samsara` nahida,
        nahida `rule` sumeru,
        scaramouche `samsara` wanderer,
        kusayla `father` dehya,
        dehya `son` kusayla,

        faruzan `teacher` collei,
        collei `student` faruzan
    ]
    [
        akademiya,
        grandharvaVille
    ]
    (Just $ RelationGraphLayout {
        rootProperty=rl (0, 15),
        elementProperties=[
            al rukkhadevataGreaterLord (-7.5, 0),
            al nahida (7.5, 0),

            al faruzan (45, -15),
            al tighnari (15, -15),
            al collei (30, -15)
        ]
    })

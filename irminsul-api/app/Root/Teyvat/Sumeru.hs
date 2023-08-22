{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya
import Root.Teyvat.Sumeru.GrandharvaVille
import Root.Teyvat.Inazuma
import Root.Teyvat.Sumeru.Akademiya.Haravatat
import Root.Teyvat.Sumeru.Akademiya.Kshahrewar
import Root.Teyvat.Sumeru.AlcazarzarayPalace
import Root.Teyvat.Sumeru.Akademiya.Rtawahist (layla)

rukkhadevataGreaterLord = "RukkhadevataGreaterLord"

nahida = "Nahida";
    buer = nahida

wanderer = "Wanderer"

dehya = "Dehya"
kusayla = "Kusayla"

nilou = "Nilou"

sumeru = clusterNode "Sumeru" Country
    [
        nahida,
        rukkhadevataGreaterLord,
        wanderer,
        dehya,
        kusayla,
        nilou
    ]
    [
        rukkhadevataGreaterLord `samsara` nahida,
        nahida `kill` rukkhadevataGreaterLord,
        nahida `rule` sumeru,
        scaramouche `samsara` wanderer,

        kusayla `father` dehya,
        dehya `son` kusayla,
        faruzan `teacher` collei,
        collei `student` faruzan,
        tighnari `friend` faruzan,
        cyno `friend` tighnari,
        cyno `friend` collei,
        cyno `friend` faruzan,
        Relation "HisJokeIsBoring" faruzan cyno,
        cyno `appreciate` alhaitham,
        cyno `esteem` nahida,
        kaveh `friend` alhaitham,

        nahida `longTimeAudience` nilou,
        collei `longTimeAudience` nilou,

        cyno `customer` dori,
        alhaitham `customer` dori,
        tighnari `customer` dori,
        layla `customer` dori
    ]
    [
        akademiya,
        grandharvaVille,
        alcazarzarayPalace
    ]
    (Just $ RelationGraphLayout {
        rootProperty=rl (-15, 15),
        elementProperties=[
            al nahida (-15, 0),
            al rukkhadevataGreaterLord (-30, 0),
            al nilou (-15, -15),
            al alhaitham (0, 15),
            al cyno (0, 0),
            al faruzan (0, -30),
            al kaveh (15, 15),
            al tighnari (30, -30),
            al collei (10, -20),
            al dori (30, 0),
            al layla (20, -10)
        ]
    })

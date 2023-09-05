{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru where

import Irminsul
import CommonRelations

import Root.Teyvat.Sumeru.GrandharvaVille
import Root.Teyvat.Inazuma
import Root.Teyvat.Sumeru.Akademiya
import Root.Teyvat.Sumeru.Akademiya.Haravatat
import Root.Teyvat.Sumeru.Akademiya.Kshahrewar
import Root.Teyvat.Sumeru.Akademiya.Rtawahist
import Root.Teyvat.Sumeru.Akademiya.Spantamad
import Root.Teyvat.Sumeru.Akademiya.Vahumana
import Root.Teyvat.Sumeru.AlcazarzarayPalace
import Root.Teyvat.Deshret.AaruVillage
import Root.Teyvat.Mondstadt.KnightsOfFavonius

rukkhadevataGreaterLord = "RukkhadevataGreaterLord"

nahida = "Nahida";
    buer = nahida

dehya = "Dehya"
kusayla = "Kusayla"

nilou = "Nilou"

sumeru = clusterNodeWithExclusion "Sumeru" Country
    ([
        nahida,
        rukkhadevataGreaterLord,
        scaramouche,
        wanderer,
        dehya,
        kusayla,
        nilou
    ],[
        lisa
    ])
    [
        rukkhadevataGreaterLord `samsara` nahida,
        nahida `kill` rukkhadevataGreaterLord,
        nahida `rule` sumeru,
        wanderer `student` nahida,
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
        collei `friend` nilou,

        cyno `customer` dori,
        alhaitham `customer` dori,
        tighnari `customer` dori,
        layla `customer` dori,

        layla `admire` alhaitham,

        candace `friend` dehya
    ]
    [
        akademiya,
        grandharvaVille,
        alcazarzarayPalace,
        aaruVillage
    ]
    (layout (-15, 15) [
        al nahida (-15, 0),
        al rukkhadevataGreaterLord (-30, 0),
        al nilou (-15, -15),
        al alhaitham (15, 15),
        al cyno (0, 0),
        al faruzan (0, -30),
        al kaveh (0, 15),
        al tighnari (30, -30),
        al collei (10, -20),
        al dori (30, 0),
        al layla (30, 15),
        al wanderer (-30, -15),
        al scaramouche (-45, -15),
        al candace (15, -5),
        al dehya (25, -15),

        cl akademiya (15, 5) (15, 5) (12, 5)
    ])

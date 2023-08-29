{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Mondstadt where

import Irminsul
import CommonRelations
import Root.Teyvat.Mondstadt.KnightsOfFavonius
import Root.Teyvat.Mondstadt.FavoniusChurch
import Root.Teyvat.Mondstadt.DawnWinery
import Root.Teyvat.Mondstadt.Lupical
import Root.Teyvat.Mondstadt.CatsTail
import Root.Teyvat.Fontaine
import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.TheFourWinds
import Root.Teyvat.Mondstadt.Springvale
import Root.Teyvat.Mondstadt.GunnhildrPegg

bennett = "Bennett"
thoma = "Thoma"
fischlVonLuftschlossNarfidort = "FischlVonLuftschlossNarfidort"
ozvaldoHrafnavins = "OzvaldoHrafnavins"

timaeus = "Timaeus"

mondstadt = clusterNode "Mondstadt" Country
    [
        dvalin,
        barbatos,
        bennett,
        thoma,
        fischlVonLuftschlossNarfidort,
        ozvaldoHrafnavins,
        monaAstrologistMegistus,
        timaeus
    ]
    [
        venti `rule` mondstadt,
        venti `allergicTo` dionaKatzlein,
        dvalin `familiar` venti,

        fischlVonLuftschlossNarfidort `friend` monaAstrologistMegistus,
        ozvaldoHrafnavins `attendant` fischlVonLuftschlossNarfidort,
        albedo `friend` monaAstrologistMegistus,
        rosaria `bewareOf` albedo,
        klee `friend` monaAstrologistMegistus,

        dilucRagvindr `youngerGeneration` jeanGunnhildr,

        bennett `partner` fischlVonLuftschlossNarfidort,
        bennett `friend` klee,
        bennett `admire` varka,
        bennett `friend` razor,

        dionaKatzlein `hate` dilucRagvindr,
        dionaKatzlein `daughter` draff,
        draff `father` dionaKatzlein,

        razor `friend` klee,
        razor `like` venti
    ]
    [
        favoniusChurch,
        knightsOfFavonius,
        dawnWinery,
        catsTail,
        lupical,
        theFourWinds,
        springvale,
        gunnhildrPegg
    ]
    (layout (-15, 0) [
        al dvalin (-7.5, 7.5),
        al venti (-15, 15),
        al bennett (15, 15),
        al fischlVonLuftschlossNarfidort (30, 15),
        al ozvaldoHrafnavins (20, 5),
        al monaAstrologistMegistus (30, 0),
        al barbaraPegg (0, -30),
        al varka (-5, -5),
        al jeanGunnhildr (-15, -30),
        al kaeyaRagvindr (-30, -30),
        al klee (0, -15),
        al albedo (30, -15),
        al dilucRagvindr (-30, -15),
        al dionaKatzlein (-30, 0),
        al razor (0, 15),
        al rosaria (30, -30),

        cl favoniusChurch (15, -22.5) (15, -22.5) (15, 5),
        cl knightsOfFavonius (-15, -15) (-15, -15) (15, 5),
        cl dawnWinery (-45, -15) (-45, -15) (12, 5),
        cl catsTail (-45, 0) (-45, 0) (12, 5),
        cl theFourWinds (-30, 15) (-30, 15) (15, 5),
        cl gunnhildrPegg (15, -30) (15, -30) (12, 5)
    ])

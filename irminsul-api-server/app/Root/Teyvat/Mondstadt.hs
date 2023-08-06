{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Mondstadt where

import Irminsul
import Shortcut
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
import Root.Teyvat.Mondstadt.Gunnhildr

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
        razor `like` venti,

        barbaraPegg `youngerSister` jeanGunnhildr,
        jeanGunnhildr `elderSister` barbaraPegg
    ]
    [
        favoniusChurch,
        knightsOfFavonius,
        dawnWinery,
        catsTail,
        lupical,
        theFourWinds,
        springvale
    ]
    (Just $ RelationGraphLayout {
        rootProperty = rl (-15, 15),
        elementProperties = [
            al dvalin (-7.5, -7.5),
            al venti (-15, 0),
            al bennett (0, 0),
            al fischlVonLuftschlossNarfidort (15, 15),
            al ozvaldoHrafnavins (7.5, 22.5),
            al monaAstrologistMegistus (30, 15),
            al barbaraPegg (0, -30),
            al varka (15, -15),
            al jeanGunnhildr (0, -15),
            al klee (15, 0),
            al albedo (30, 0),
            al dilucRagvindr (-15, -15),
            al kaeyaRagvindr (-15, -30),
            al dionaKatzlein (-30, -15),
            al razor (0, 15),

            cl knightsOfFavonius (17, -30) (15, -30) (15, 5),
            cl catsTail (-45, -15) (-45, -15) (15, 5),
            cl dawnWinery (-30, -30) (-30, -30) (15, 5),
            cl favoniusChurch (0, -40) (0, -40) (15, 5),
            cl theFourWinds (-30, 0) (-30, 0) (15, 5)
        ]
    })

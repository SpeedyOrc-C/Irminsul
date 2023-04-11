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
import Root.Teyvat.Inazuma (scaramouche)

venti = ach "Venti";
    barbatos = venti
dvalin = ach "Dvalin"

bennett = ach "Bennett"
thoma = ach "Thoma"
fischlVonLuftschlossNarfidort = ach "FischlVonLuftschlossNarfidort"
ozvaldoHrafnavins = ach "OzvaldoHrafnavins"

draff = ach "Draff"

mondstadt = clusterNode "Mondstadt" Country
    [
        dvalin,
        barbatos,
        bennett,
        thoma,
        fischlVonLuftschlossNarfidort,
        ozvaldoHrafnavins,
        monaAstrologistMegistus
    ]
    [
        venti `rule` mondstadt,
        venti `dislike` dionaKatzlein,
        dvalin `familiar` venti,

        fischlVonLuftschlossNarfidort `friend` monaAstrologistMegistus,
        ozvaldoHrafnavins `attendant` fischlVonLuftschlossNarfidort,
        albedo `friend` monaAstrologistMegistus,
        klee `friend` monaAstrologistMegistus,
        -- scaramouche `enemy` monaAstrologistMegistus,

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
        lupical
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
            cl catsTail (-30, 0) (-30, 0) (15, 5)
        ]
    })

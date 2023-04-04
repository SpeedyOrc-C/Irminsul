module Root.Teyvat.Mondstadt where

import Irminsul
import Shortcut
import CommonRelations
import Root.Teyvat.Mondstadt.KnightsOfFavonius
import Root.Teyvat.Mondstadt.FavoniusChurch
import Root.Teyvat.Fontaine
import Root.Teyvat.Mondstadt.DawnWinery
import Root.Teyvat.Khaenriah
import Root.Teyvat.Inazuma (scaramouche)

venti = ach "venti";
    barbatos = venti

dvalin = ach "Dvalin"

razor = ach "Razor"
bennett = ach "Bennett"
thoma = ach "Thoma"
fischlVonLuftschlossNarfidort = ach "FischlVonLuftschlossNarfidort"

mondstadt = clusterNode "Mondstadt" Country
    [
        barbatos,
        razor,
        bennett,
        thoma,
        fischlVonLuftschlossNarfidort,
        monaAstrologistMegistus
    ]
    [
        dvalin `familiar` venti,

        fischlVonLuftschlossNarfidort `friend` monaAstrologistMegistus,
        albedo `friend` monaAstrologistMegistus,
        klee `friend` monaAstrologistMegistus,
        scaramouche `enemy` monaAstrologistMegistus,

        dilucRagvindr `youngerGeneration` jeanGunnhildr,

        bennett `partner` fischlVonLuftschlossNarfidort,
        bennett `friend` klee,
        bennett `admire` varka,
        bennett `friend` razor,

        barbaraGunnhildr `youngerSister` jeanGunnhildr,
        jeanGunnhildr `elderSister` barbaraGunnhildr
    ]
    [
        favoniusChurch,
        knightsOfFavonius,
        dawnWinery
    ]
    Nothing

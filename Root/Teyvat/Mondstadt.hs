{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Mondstadt where

import Irminsul
import CommonRelations
import Root.Teyvat.Mondstadt.KnightsOfFavonius (knightsOfFavonius, jeanGunnhildr)
import Root.Teyvat.Mondstadt.FavoniusChurch (favoniusChurch, barbaraGunnhildr)
import Root.Teyvat.Fontaine (monaAstrologistMegistus)

venti = ach "venti";
    barbatos = venti

amber = ach "Amber"
lisa = ach "Lisa"
sucrose = ach "Sucrose"
razor = ach "Razor"
bennett = ach "Bennett"
thoma = ach "Thoma"
fischlVonLuftschlossNarfidort = ach "FischlVonLuftschlossNarfidort"

mondstadt = clusterNode "Mondstadt" Country
    [
        barbatos,
        amber,
        lisa,
        sucrose,
        razor,
        bennett,
        thoma,
        fischlVonLuftschlossNarfidort,
        monaAstrologistMegistus
    ]
    [
        barbaraGunnhildr `youngerSister` jeanGunnhildr,
        jeanGunnhildr `elderSister` barbaraGunnhildr
    ]
    [
        favoniusChurch,
        knightsOfFavonius
    ]

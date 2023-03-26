{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Mondstadt where

import Irminsul
import Shortcut
import CommonRelations
import Root.Teyvat.Mondstadt.KnightsOfFavonius (knightsOfFavonius, jeanGunnhildr)
import Root.Teyvat.Mondstadt.FavoniusChurch (favoniusChurch, barbaraGunnhildr)
import Root.Teyvat.Fontaine (monaAstrologistMegistus)
import Root.Teyvat.Mondstadt.DawnWinery (dawnWinery)

venti = ach "venti";
    barbatos = venti

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
        barbaraGunnhildr `youngerSister` jeanGunnhildr,
        jeanGunnhildr `elderSister` barbaraGunnhildr
    ]
    [
        favoniusChurch,
        knightsOfFavonius,
        dawnWinery
    ]
    Nothing

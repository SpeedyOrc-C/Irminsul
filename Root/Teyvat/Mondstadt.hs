{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Mondstadt where

import Irminsul
import CommonRelations
import Root.Teyvat.Mondstadt.KnightsOfFavonius (knightsOfFavonius, jeanGunnhildr)
import Root.Teyvat.Mondstadt.FavoniusChurch (favoniusChurch, barbaraGunnhildr)
import Root.Teyvat.Fontaine (monaAstrologistMegistus)

venti = Atom "venti" Character;
    barbatos = venti

amber = Atom "Amber" Character
lisa = Atom "Lisa" Character
sucrose = Atom "Sucrose" Character
razor = Atom "Razor" Character
bennett = Atom "Bennett" Character
thoma = Atom "Thoma" Character
fischlVonLuftschlossNarfidort = Atom "FischlVonLuftschlossNarfidort" Character

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

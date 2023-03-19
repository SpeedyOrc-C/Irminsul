{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Mondstadt where

import Irminsul
import Root.Teyvat.Mondstadt.KnightsOfFavonius (knightsOfFavonius, jeanGunnhildr)
import Root.Teyvat.Mondstadt.FavoniusChurch (favoniusChurch, barbaraGunnhildr)

venti = Atom "Venti" Character

amber = Atom "Amber" Character
lisa = Atom "Lisa" Character
sucrose = Atom "Sucrose" Character
razor = Atom "Razor" Character
bennett = Atom "Bennett" Character
thoma = Atom "Thoma" Character
fischlVonLuftschlossNarfidort = Atom "FischlVonLuftschlossNarfidort" Character

thisAtoms = [
    venti,
    amber,
    lisa,
    sucrose,
    razor,
    bennett,
    thoma,
    fischlVonLuftschlossNarfidort
    ]

thisRelations = [
    barbaraGunnhildr `youngerSister` jeanGunnhildr
    ]

thisChildClusters = [
    favoniusChurch,
    knightsOfFavonius
    ]

mondstadt = mergeFromChildClusters
    (Cluster "Mondstadt" Country thisAtoms thisRelations)
    thisChildClusters

index = generateIndex thisChildClusters

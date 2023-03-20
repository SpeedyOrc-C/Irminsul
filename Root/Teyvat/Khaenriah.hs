{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Khaenriah where

import Irminsul
import CommonRelations

albedo = Atom "Albedo" Character
rhinedottir = Atom "Rhinedottir" Character

kaeyaAlberich = Atom "KaeyaAlberich" Character
chlotharAlberich = Atom "ChlotharAlberich" Character

dainsleif = Atom "Dainsleif" Character

khaenriah = clusterNode "Khaenriah" Country
    [
        albedo,
        rhinedottir,
        kaeyaAlberich,
        chlotharAlberich,
        dainsleif
    ]
    [
        rhinedottir `create` albedo
    ]
    [
    ]

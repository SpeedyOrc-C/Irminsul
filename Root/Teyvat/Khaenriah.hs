{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Khaenriah where

import Irminsul
import CommonRelations

albedo = ach "Albedo"
rhinedottir = ach "Rhinedottir"

kaeyaAlberich = ach "KaeyaAlberich"
chlotharAlberich = ach "ChlotharAlberich"

dainsleif = ach "Dainsleif"

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

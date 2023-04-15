module Root.Teyvat.Khaenriah where

import Irminsul
import Shortcut
import CommonRelations
import Root.Teyvat.Khaenriah.Schwaneritter

albedo = ach "Albedo"
rhinedottir = ach "Rhinedottir"

kaeyaAlberich = ach "KaeyaAlberich"
chlotharAlberich = ach "ChlotharAlberich"

dainsleif = ach "Dainsleif"

irmin = ach "Irmin"


khaenriah = clusterNode "Khaenriah" Country
    [
        albedo,
        rhinedottir,
        kaeyaAlberich,
        chlotharAlberich,
        dainsleif,
        irmin
    ]
    [
        rhinedottir `create` albedo
    ]
    [
        schwaneritter
    ]
    Nothing

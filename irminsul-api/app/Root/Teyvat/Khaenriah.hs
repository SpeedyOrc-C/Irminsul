module Root.Teyvat.Khaenriah where

import Irminsul
import CommonRelations
import Root.Teyvat.Khaenriah.Schwaneritter

albedo = "Albedo"
rhinedottir = "Rhinedottir"

kaeyaAlberich = "KaeyaAlberich"
chlotharAlberich = "ChlotharAlberich"

dainsleif = "Dainsleif"

irmin = "Irmin"


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

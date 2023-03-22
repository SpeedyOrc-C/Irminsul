module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import CommonRelations (daughter)

import Root.AnotherWorld (travellerArchonSide)
import Root.Teyvat.Khaenriah (kaeyaAlberich)

noelle = ach "Noelle"
jeanGunnhildr = ach "JeanGunnhildr"
eulaLaurence = ach "EulaLaurence"
klee = ach "Klee"

knightsOfFavonius = clusterNode "KnightsOfFavonius" Organization
    [
        noelle,
        jeanGunnhildr,
        eulaLaurence,
        klee,
        kaeyaAlberich,
        travellerArchonSide
    ]
    [
    ]
    [

    ]

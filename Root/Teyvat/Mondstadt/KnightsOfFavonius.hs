module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import CommonRelations (daughter)
import Root.AnotherWorld (travellerArchonSide)

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
        travellerArchonSide
    ]
    [
    ]
    [

    ]

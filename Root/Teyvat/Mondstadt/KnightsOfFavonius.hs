module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import CommonRelations (daughter)
import Root.AnotherWorld (travellerArchonSide)

noelle = Atom "Noelle" Character
jeanGunnhildr = Atom "JeanGunnhildr" Character
eulaLaurence = Atom "EulaLaurence" Character
klee = Atom "Klee" Character

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

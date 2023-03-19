module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul

noelle = Atom "Noelle" Character
jeanGunnhildr = Atom "JeanGunnhildr" Character
eulaLaurence = Atom "EulaLaurence" Character

thisAtoms = [
    noelle,
    jeanGunnhildr,
    eulaLaurence
    ]

thisRelations = [
    
    ]

knightsOfFavonius = Cluster
    "KnightsOfFavonius" Organization thisAtoms thisRelations

index = ClusterIndex []
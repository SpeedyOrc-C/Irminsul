module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import Shortcut
import CommonRelations

import Root.AnotherWorld
import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam
import Root.Teyvat.Mondstadt.FavoniusChurch
import Root.Teyvat.Kaeya

varka = ach "Varka"
jeanGunnhildr = ach "JeanGunnhildr"

lisa = ach "Lisa"
amber = ach "Amber"
klee = ach "Klee"
sucrose = ach "Sucrose"
noelle = ach "Noelle"

knightsOfFavonius = clusterNode "KnightsOfFavonius" Organization
    [
        jeanGunnhildr,
        lisa,
        kaeyaAlberich,
        amber,
        klee,
        albedo,
        sucrose,
        noelle,
        travellerArchonSide
    ]
    [
        jeanGunnhildr `actingGrandMaster` knightsOfFavonius,
        lisa `friend` jeanGunnhildr,
        amber `friend` eulaLawrence,
        albedo `teacher` sucrose,
        sucrose `student` albedo,
        foster elderBrother albedo klee,
        foster youngerSister klee albedo,
        acting guardian jeanGunnhildr klee
    ]
    [
        guerillaTeam
    ]
    (Just $ Layout {
        rootProperty =
            rl (-37, 15) (-37, 15),
        elementProperties = [
            al jeanGunnhildr (-15, 15),
            al klee (-15, 0),
            al albedo (-15, -15),
            al sucrose (0, -15),
            al lisa (0, 15),

            al kaeyaAlberich (15, 0),
            al noelle (15, -15),

            al amber (15, 15),
            al eulaLawrence (30, 15),
            al mikaSchmidt (30, 0),

            cl guerillaTeam (30, 7.5) (37.5, 15) 15 30
        ]
    })

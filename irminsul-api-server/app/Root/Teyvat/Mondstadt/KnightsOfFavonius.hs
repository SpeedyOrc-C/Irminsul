module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import Shortcut
import CommonRelations

import Root.AnotherWorld
import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam
import Root.Teyvat.Mondstadt.DawnWinery

varka = ach "Varka"
jeanGunnhildr = ach "JeanGunnhildr"

lisa = ach "Lisa"
amber = ach "Amber"
klee = ach "Klee"
sucrose = ach "Sucrose"
noelle = ach "Noelle"

knightsOfFavonius = clusterNode "KnightsOfFavonius" Organization
    [
        varka,
        jeanGunnhildr,
        lisa,
        kaeyaRagvindr,
        amber,
        klee,
        albedo,
        sucrose,
        noelle,
        travellerArchonSide
    ]
    [
        varka `grandMaster` knightsOfFavonius,
        jeanGunnhildr `actingGrandMaster` knightsOfFavonius,
        lisa `friend` jeanGunnhildr,
        amber `friend` eulaLawrence,
        albedo `teacher` sucrose,
        sucrose `student` albedo,
        foster elderBrother albedo klee,
        foster youngerSister klee albedo,
        acting guardian jeanGunnhildr klee,
        ra "SupportInSilence" kaeyaAlberich noelle
    ]
    [
        guerillaTeam
    ]
    (Just $ RelationGraphLayout {
        rootProperty = rl (-37, 15),
        elementProperties = [
            al varka (-22.5, 7.5),
            al jeanGunnhildr (-15, 15),
            al lisa (-15, 0),
            al kaeyaAlberich (0, 0),
            al amber (-15, -15),
            al klee (0, 15),
            al albedo (15, 15),
            al sucrose (30, 15),
            al noelle (15, 0),
            al eulaLawrence (0, -15),
            al mikaSchmidt (30, -15),

            cl guerillaTeam (18.5, -15) (18, -15) (15, 5)
        ]
    })

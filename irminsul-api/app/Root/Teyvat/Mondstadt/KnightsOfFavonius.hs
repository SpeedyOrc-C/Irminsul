module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import CommonRelations

import Root.AnotherWorld
import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam
import Root.Teyvat.Mondstadt.DawnWinery
import Root.Teyvat.Mondstadt.GunnhildrPegg

varka = "Varka"

lisa = "Lisa"
amber = "Amber"
klee = "Klee"
sucrose = "Sucrose"
noelle = "Noelle"

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
        sucrose `assistant` albedo,
        foster elderBrother albedo klee,
        foster youngerSister klee albedo,
        acting guardian jeanGunnhildr klee,
        Relation "SupportInSilence" kaeyaRagvindr noelle
    ]
    [
        guerillaTeam
    ]
    (layout (-37, 15) [
        al varka (-37, 0),
        al jeanGunnhildr (-15, 15),
        al lisa (-15, 0),
        al kaeyaRagvindr (0, 0),
        al amber (-15, -15),
        al klee (0, 15),
        al albedo (15, 15),
        al sucrose (30, 15),
        al noelle (15, 0),
        al eulaLawrence (0, -15),
        al mikaSchmidt (30, -15),
        al travellerArchonSide (-30, -15),

        cl guerillaTeam (15.5, -15) (15.5, -15) (12, 5)
    ])

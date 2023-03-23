module Root.Teyvat.Mondstadt.KnightsOfFavonius where

import Irminsul
import Shortcut
import CommonRelations

import Root.AnotherWorld (travellerArchonSide)
import Root.Teyvat.Khaenriah (kaeyaAlberich, albedo)
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam
    ( eulaLaurence, guerillaTeam )
import Root.Teyvat.Mondstadt.FavoniusChurch (barbaraGunnhildr)

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
        acting captain jeanGunnhildr knightsOfFavonius,
        lisa `friend` jeanGunnhildr,
        amber `friend` eulaLaurence,
        albedo `teacher` sucrose,
        sucrose `student` albedo,
        foster elderBrother albedo klee,
        foster youngerSister klee albedo,
        acting guardian jeanGunnhildr klee
    ]
    [
        guerillaTeam
    ]

module Root.Teyvat where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Mondstadt
import Root.Teyvat.Liyue 
import Root.Teyvat.Inazuma 
import Root.Teyvat.Snezhnaya 
import Root.Teyvat.Hexenzirkel
import Root.Teyvat.Mondstadt.KnightsOfFavonius 
import Root.Teyvat.Khaenriah 
import Root.Teyvat.Liyue.WangshengFuneralParlor (zhongli)
import Root.Teyvat.Sumeru

heavenlyPrinciple = ao "HeavenlyPrinciple"
sustainerOfHeavenlyPrinciple = ach "SustainerOfHeavenlyPrinciple"

teyvat = clusterNode "Teyvat" World
    [
        heavenlyPrinciple,
        sustainerOfHeavenlyPrinciple
    ]
    [
        alice `mother` klee,
        klee `daughter` alice,

        venti `friend` zhongli,
        venti `friend` buer
    ]
    [
        mondstadt,
        liyue,
        inazuma,
        snezhnaya,
        khaenriah,
        hexenzirkel
    ]
    (Just $ RelationGraphLayout {
        rootProperty= rl (0, 0),
        elementProperties=[
            cl mondstadt (80, 80) (80, 80) (60, 60),
            cl liyue (0, 0) (0, 0) (60, 60),
            cl inazuma (80, -80) (80, -80) (60, 60)
        ]
    })

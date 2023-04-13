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
import Root.Teyvat.Liyue.WangshengFuneralParlor
import Root.Teyvat.Sumeru
import Root.Teyvat.Mondstadt.CatsTail
import Root.Teyvat.Liyue.WanminRestaurant
import Root.Teyvat.Mondstadt.TheFourWinds

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
        venti `friend` buer,

        dionaKatzlein `friend` xiangling
    ]
    [
        mondstadt,
        liyue,
        inazuma,
        snezhnaya,
        khaenriah,
        hexenzirkel
    ]
    Nothing

{-# LANGUAGE OverloadedStrings #-}

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
import Root.Teyvat.Liyue.Qixing
import Root.Teyvat.Fontaine
import Root.Teyvat.Mondstadt.Gunnhildr

heavenlyPrinciple = ao "HeavenlyPrinciple"
sustainerOfHeavenlyPrinciple = "SustainerOfHeavenlyPrinciple"

teyvat = clusterNode "Teyvat" World
    [
        heavenlyPrinciple,
        sustainerOfHeavenlyPrinciple
    ]
    [
        alice `mother` klee,
        klee `daughter` alice,
        alice `friend` idyia,

        venti `friend` zhongli,
        venti `friend` buer,

        dionaKatzlein `friend` xiangling,

        ningguang `appreciate` jeanGunnhildr,
        ningguang `partner` jeanGunnhildr
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
        rootProperty=rl (-15, 15),
        elementProperties=[
            cl mondstadt (15, 15) (15, 15) (15, 5),
            cl liyue (0, 0) (0, 0) (15, 5),
            cl inazuma (15, -15) (15, -15) (15, 5)
        ]
    })

module Root.Teyvat where

import Irminsul
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
import Root.Teyvat.Mondstadt.GunnhildrPegg
import Root.Teyvat.Deshret
import Root.Teyvat.Byakuyakoku
import Root.Teyvat.Sumeru.GrandharvaVille

heavenlyPrinciple = "HeavenlyPrinciple"
sustainerOfHeavenlyPrinciple = "SustainerOfHeavenlyPrinciple"

teyvat = clusterNode "Teyvat" World
    [
        heavenlyPrinciple,
        sustainerOfHeavenlyPrinciple
    ]
    [
        collei `friend` amber,

        alice `mother` klee,
        klee `daughter` alice,
        alice `friend` idyia,

        venti `friend` zhongli,
        venti `friend` buer,

        dionaKatzlein `friend` xiangling,

        ningguang `appreciate` jeanGunnhildr,
        ningguang `partner` jeanGunnhildr,

        eiRaiden `kill` orobaxi
    ]
    [
        mondstadt,
        liyue,
        inazuma,
        sumeru,
        deshret,
        fontaine,
        snezhnaya,
        khaenriah,
        hexenzirkel,
        byakuyakoku
    ]
    (layout (0, 21) [
        al collei (3, 6),
        al amber (22.5, 13),
        al eiRaiden (24, -30.5),
        al orobaxi (6, -30.5),

        cl mondstadt (24, 20) (24, 20) (12, 5),
        cl liyue (14, 0) (14, 0) (12, 5),
        cl inazuma (24, -20) (24, -20) (12, 5),
        cl sumeru (-6, 0) (-6, 0) (12, 5),
        cl deshret (-26, 0) (-26, 0) (12, 5),
        cl fontaine (-26, 20) (-26, 20) (12, 5),
        cl khaenriah (-41, 7.5) (-41, 7.5) (12, 5),
        cl byakuyakoku (6, -20) (6, -20) (12, 5)
    ])

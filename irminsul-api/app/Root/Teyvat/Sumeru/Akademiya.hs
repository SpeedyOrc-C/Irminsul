{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru.Akademiya where

import Irminsul
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya.Haravatat
import Root.Teyvat.Sumeru.Akademiya.Rtawahist
import Root.Teyvat.Sumeru.Akademiya.Kshahrewar

alhaitham = "Alhaitham"
cyno = "Cyno"

akademiya = clusterNode "Akademiya" Organization
    [
        alhaitham,
        cyno
    ]
    [

    ]
    [
        haravatat,
        rtawahist,
        kshahrewar
    ]
    (layout (0, 20) [
        cl haravatat (-37.5, 7.5) (-37.5, 7.5) (12, 4),
        al faruzan (-37.5, 0),

        cl rtawahist (-22.5, 7.5) (-22.5, 7.5) (12, 4),
        al layla (-22.5, 0),

        cl kshahrewar (-7.5, 7.5) (-7.5, 7.5) (12, 4),
        al kaveh (-7.5, 0)
    ])

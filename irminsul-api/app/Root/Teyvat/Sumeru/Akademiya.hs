{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru.Akademiya where

import Irminsul
import Shortcut
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
    Nothing

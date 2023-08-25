{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti where
-- 仙人

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Liyue.Adepti.Yaksha
import Root.Teyvat.Liyue.Adepti.SeaGazersAbode
import Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode
import Root.Teyvat.Liyue.Adepti.CloudRetainersAbode

moonCarver = "MoonCarver"
mountainShaper = "MountainShaper"


adepti = clusterNode "Adepti" Organization
    [
        moonCarver,
        mountainShaper
    ]
    [
        acting guardian ganyu yaoyao
    ]
    [
        yaksha,
        cloudRetainersAbode,
        streetwardRamblersAbode,
        seaGazersAbode
    ]
    (Just $ RelationGraphLayout {
        rootProperty=rl (0, 0),
        elementProperties=[
            al cloudRetainer (15, 0),
            al ganyu (30, 0),
            al streetwardRambler (0, -15),
            al yaoyao (15, -15),
            al moonCarver (-15, 0),
            al mountainShaper (0, 15),
            al xiao (-30, 0),

            cl yaksha (-30, 15) (-30, 15) (12, 5)
        ]
    })
{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti where
-- 仙人

import Irminsul
import Shortcut

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

    ]
    [
        yaksha,
        cloudRetainersAbode,
        streetwardRamblersAbode,
        seaGazersAbode
    ]
    Nothing
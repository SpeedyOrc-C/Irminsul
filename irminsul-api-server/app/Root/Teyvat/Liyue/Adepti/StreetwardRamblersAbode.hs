{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode where

import Irminsul
import Shortcut

streetwardRambler = "StreetwardRambler"
yaoyao = "Yaoyao"

streetwardRamblersAbode = clusterLeaf "StreetwardRamblersAbode" Organization
    [
        streetwardRambler,
        yaoyao
    ]
    [

    ]
    Nothing

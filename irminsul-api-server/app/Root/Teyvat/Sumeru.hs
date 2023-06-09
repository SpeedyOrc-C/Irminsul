{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya
import Root.Teyvat.Sumeru.GrandharvaVille
import Root.Teyvat.Inazuma

nahida = "Nahida";
    buer = nahida

wanderer = "Wanderer"
dehya = "Dehya"

sumeru = clusterNode "Sumeru" Country
    [
        nahida,
        wanderer,
        dehya
    ]
    [
        scaramouche `samsara` wanderer
    ]
    [
        akademiya,
        grandharvaVille
    ]
    Nothing

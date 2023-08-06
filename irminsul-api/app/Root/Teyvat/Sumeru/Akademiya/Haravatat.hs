{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru.Akademiya.Haravatat where

import Irminsul
import CommonRelations
import Shortcut

import Root.Teyvat.Sumeru.GrandharvaVille

faruzan = "Faruzan"

haravatat = clusterLeaf "Haravatat" Organization
    [
        faruzan
    ]
    [
        faruzan `teacher` collei
    ]
    Nothing

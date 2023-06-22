{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Sumeru.GrandharvaVille where

import Irminsul
import Shortcut
import CommonRelations


tighnari = "Tighnari"
collei = "Collei"

grandharvaVille = clusterLeaf "GrandharvaVille" Area
    [
        tighnari,
        collei
    ]
    [
        tighnari `teacher` collei
    ]
    Nothing

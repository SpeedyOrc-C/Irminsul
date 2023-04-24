{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.WatatsumiIsland where

import Irminsul
import Shortcut

kokomiSangonomiya = "KokomiSangonomiya"
gorou = "Gorou"

watatsumiIsland = clusterLeaf "WatatsumiIsland" Area
    [
        kokomiSangonomiya,
        gorou
    ]
    [
    ]
    Nothing

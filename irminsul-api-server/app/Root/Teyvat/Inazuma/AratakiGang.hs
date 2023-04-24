{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.AratakiGang where

import Irminsul
import Shortcut

shinobuKuki = "ShinobuKuki"
ittoArataki = "IttoArataki"

aratakiGang = clusterLeaf "AratakiGang" Organization
    [
        shinobuKuki,
        ittoArataki
    ]
    [
    ]
    Nothing

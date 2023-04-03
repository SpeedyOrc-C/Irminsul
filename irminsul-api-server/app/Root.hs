{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root where

import Irminsul
import Shortcut
import Root.Teyvat

alloy = ach "Alloy"
rost = ach "Rost"

root = clusterNode "Root" Root
    [
        
    ]
    [

    ]
    [
        teyvat
    ]
    Nothing

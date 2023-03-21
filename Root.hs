{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root where

import Irminsul
import Root.Teyvat (teyvat)

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

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root where

import Irminsul
import Root.Teyvat (teyvat)

alloy = Atom "Alloy" Character
rost = Atom "Rost" Character

root = clusterNode "Root" Root
    [
        
    ]
    [

    ]
    [
        teyvat
    ]

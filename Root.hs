{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root where

import Irminsul
import Root.Teyvat (teyvat)

lumine = Atom "Lumine" Character
aether = Atom "Aether" Character

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
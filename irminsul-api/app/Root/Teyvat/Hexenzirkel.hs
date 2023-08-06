{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Hexenzirkel where

import Irminsul
import Shortcut
import Root.Teyvat.Khaenriah
import CommonRelations

alice = "Alice"
iIvanovnaN = "IIvanonaN"
andersdotter = "Andersdotter"
barbeloth = "Barbeloth"

hexenzirkel = clusterLeaf "Hexenzirkel" Organization
    [
        alice,
        rhinedottir,
        iIvanovnaN,
        andersdotter,
        barbeloth
    ]
    [
        
    ]
    Nothing

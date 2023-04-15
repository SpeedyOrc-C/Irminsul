module Root.Teyvat.Hexenzirkel where

import Irminsul
import Shortcut
import Root.Teyvat.Khaenriah
import CommonRelations

alice = ach "Alice"
iIvanovnaN = ach "IIvanonaN"
andersdotter = ach "Andersdotter"
barbeloth = ach "Barbeloth"

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

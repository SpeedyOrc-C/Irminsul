module Root.Teyvat.Hexenzirkel where

import Irminsul
import Root.Teyvat.Khaenriah (rhinedottir)
import CommonRelations (mother)

alice = Atom "Alice" Character
iIvanovnaN = Atom "IIvanonaN" Character
andersdotter = Atom "Andersdotter" Character
barbeloth = Atom "Barbeloth" Character


hexenzirkel = clusterNode "Hexenzirkel" Organization
    [
        alice,
        rhinedottir,
        iIvanovnaN,
        andersdotter,
        barbeloth
    ]
    [
    ]
    [
        
    ]
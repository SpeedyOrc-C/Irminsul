module Root.Teyvat.Snezhnaya.Fatui where

import Irminsul
import Root.Teyvat.Inazuma (scaramouche)

tartaglia = Atom "Tartaglia" Character;
    childe = tartaglia

fatui = clusterNode "Fatui" Organization
    [
        childe,
        scaramouche
    ]
    [

    ]
    [

    ]

module Root.Teyvat.Snezhnaya.Fatui where

import Irminsul
import Shortcut
import Root.Teyvat.Inazuma (scaramouche)

tartaglia = ach "Tartaglia";
    childe = tartaglia

rosalyneKruzchkaLohefalter = ach "RosalyneKruzchkaLohefalter";
    signora = rosalyneKruzchkaLohefalter

fatui = clusterNode "Fatui" Organization
    [
        childe,
        scaramouche,
        signora
    ]
    [

    ]
    [

    ]
    Nothing

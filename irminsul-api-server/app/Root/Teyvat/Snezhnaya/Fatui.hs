{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Snezhnaya.Fatui where

import Irminsul
import Shortcut
import Root.Teyvat.Inazuma (scaramouche)

tartaglia = "Tartaglia";
    childe = tartaglia

rosalyneKruzchkaLohefalter = "RosalyneKruzchkaLohefalter";
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

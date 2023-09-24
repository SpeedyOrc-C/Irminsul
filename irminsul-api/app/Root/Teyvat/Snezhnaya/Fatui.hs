{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Snezhnaya.Fatui where

import Irminsul
import Root.Teyvat.Inazuma
import Root.Teyvat.Snezhnaya.Fatui.HouseOfTheHearth

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
        houseOfTheHearth
    ]
    Nothing

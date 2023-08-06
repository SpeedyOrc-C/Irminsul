{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Fontaine where

import Irminsul
import Shortcut
import Root.Teyvat.Fontaine.TheSteambird 

monaAstrologistMegistus = "MonaAstrologistMegistus"

idyia = "Idyia"
rhodeia = "Rhodeia"

fontaine = clusterNode "Fontaine" Country
    [
        monaAstrologistMegistus,
        idyia,
        rhodeia
    ]
    [

    ]
    [
        theSteambird
    ]
    Nothing

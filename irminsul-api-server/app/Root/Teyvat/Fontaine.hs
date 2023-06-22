{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Fontaine where

import Irminsul
import Shortcut
import Root.Teyvat.Fontaine.TheSteambird 

monaAstrologistMegistus = "MonaAstrologistMegistus"

fontaine = clusterNode "Fontaine" Country
    [
        monaAstrologistMegistus
    ]
    [

    ]
    [
        theSteambird
    ]
    Nothing

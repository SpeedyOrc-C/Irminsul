{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Fontaine where

import Irminsul
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
    (layout (0, 0) [
        
    ])

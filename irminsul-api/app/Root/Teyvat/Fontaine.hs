module Root.Teyvat.Fontaine where

import Irminsul
import Root.Teyvat.Fontaine.TheSteambird 
import CommonRelations

monaAstrologistMegistus = "MonaAstrologistMegistus"

idyia = "Idyia"
rhodeia = "Rhodeia"

furina = "Furina"
forcalos = "Forcalos"
neuvillette = "Neuvillette"

fontaine = clusterNode "Fontaine" Country
    [
        neuvillette,
        furina,
        forcalos,

        monaAstrologistMegistus,
        idyia,
        rhodeia
    ]
    [
        neuvillette `rule` fontaine,
        fontaine `kill` forcalos,
        forcalos `create` furina,
        Relation "ReturnPower" forcalos neuvillette
    ]
    [
        theSteambird
    ]
    (layout (0, 15) [
        al neuvillette (-15, 0),
        al forcalos (0, 0),
        al furina (0, -15),
        al charlotte (15, -15),

        cl theSteambird (15, 0) (15, 0) (15, 5)
    ])

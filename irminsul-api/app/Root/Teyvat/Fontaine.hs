module Root.Teyvat.Fontaine where

import Irminsul
import Root.Teyvat.Fontaine.TheSteambird 
import CommonRelations
import Root.Teyvat.Fontaine.LLF

monaAstrologistMegistus = "MonaAstrologistMegistus"
navia = "Navia"
chevreuse = "Chevreuse"

idyia = "Idyia"
rhodeia = "Rhodeia"

furina = "Furina"
forcalos = "Forcalos"
neuvillette = "Neuvillette"
wriothesley = "Wriothesley"

fontaine = clusterNode "Fontaine" Country
    [
        neuvillette,
        wriothesley,
        furina,
        forcalos,

        monaAstrologistMegistus,
        idyia,
        rhodeia,

        navia,
        chevreuse
    ]
    [
        neuvillette `rule` fontaine,
        fontaine `kill` forcalos,
        forcalos `create` furina,
        Relation "ReturnPower" forcalos neuvillette
    ]
    [
        theSteambird,
        llf
    ]
    (layout (0, 15) [
        al neuvillette (-15, 0),
        al wriothesley (-15, 15),
        al forcalos (0, 0),
        al furina (0, -15),
        al charlotte (15, -15),

        al freminet (-30, 15),
        al lyney (-30, 0),
        al lynette (-30, -15),

        al navia (-15, -15),
        al chevreuse (15, 15),

        cl theSteambird (15, -7.5) (15, 0) (15, 5)
    ])

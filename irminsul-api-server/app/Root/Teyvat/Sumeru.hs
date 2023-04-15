module Root.Teyvat.Sumeru where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya
import Root.Teyvat.Sumeru.GrandharvaVille

nahida = ach "Nahida";
    buer = nahida

wanderer = ach "Wanderer"

sumeru = clusterNode "Sumeru" Country
    [
        nahida,
        wanderer
    ]
    [

    ]
    [
        akademiya,
        grandharvaVille
    ]
    Nothing

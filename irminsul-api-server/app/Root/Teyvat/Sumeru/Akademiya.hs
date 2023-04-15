module Root.Teyvat.Sumeru.Akademiya where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya.Haravatat
import Root.Teyvat.Sumeru.Akademiya.Rtawahist

alhaitham = ach "Alhaitham"

akademiya = clusterNode "Akademiya" Organization
    [
        alhaitham
    ]
    [

    ]
    [
        haravatat,
        rtawahist
    ]
    Nothing

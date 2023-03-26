module Root.Teyvat where

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Mondstadt (mondstadt)
import Root.Teyvat.Liyue (liyue)
import Root.Teyvat.Inazuma (inazuma)
import Root.Teyvat.Snezhnaya (snezhnaya)
import Root.Teyvat.Hexenzirkel (alice, hexenzirkel)
import Root.Teyvat.Mondstadt.KnightsOfFavonius (klee)
import Root.Teyvat.Khaenriah (khaenriah)

teyvat = clusterNode "Teyvat"
    World
    [
    ]
    [
        alice `mother` klee,
        klee `daughter` alice
    ]
    [
        mondstadt,
        liyue,
        inazuma,
        snezhnaya,
        khaenriah,
        hexenzirkel
    ]
    Nothing

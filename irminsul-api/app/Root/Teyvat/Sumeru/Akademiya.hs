module Root.Teyvat.Sumeru.Akademiya where

import Irminsul
import CommonRelations

import Root.Teyvat.Sumeru.Akademiya.Haravatat
import Root.Teyvat.Sumeru.Akademiya.Rtawahist
import Root.Teyvat.Sumeru.Akademiya.Kshahrewar
import Root.Teyvat.Sumeru.Akademiya.Amurta
import Root.Teyvat.Sumeru.Akademiya.Spantamad
import Root.Teyvat.Sumeru.Akademiya.Vahumana
import Root.Teyvat.Sumeru.GrandharvaVille
import Root.Teyvat.Mondstadt.KnightsOfFavonius

akademiya = clusterNode "Akademiya" Organization
    [
        alhaitham,
        cyno
    ]
    [

    ]
    [
        amurta,
        haravatat,
        kshahrewar,
        rtawahist,
        spantamad,
        vahumana
    ]
    (layout (0, 20) [
        cl rtawahist (-37.5, 7.5) (-37.5, 7.5) (12, 4),
        al layla (-37.5, 0),

        cl haravatat (-22.5, 7.5) (-22.5, 7.5) (12, 4),
        al alhaitham (-22.5, 0),
        al faruzan (-22.5, -7.5),

        cl kshahrewar (-7.5, 7.5) (-7.5, 7.5) (12, 4),
        al kaveh (-7.5, 0),

        cl amurta (7.5, 7.5) (7.5, 7.5) (12, 4),
        al tighnari (7.5, 0),

        cl spantamad (22.5, 7.5) (22.5, 7.5) (12, 4),
        al cyno (22.5, 0),
        al lisa (22.5, -7.5),

        cl vahumana (37.5, 7.5) (37.5, 7.5) (12, 4),
        al wanderer (37.5, 0)
    ])

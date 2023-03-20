module Root.Teyvat where

import Irminsul
import Root.Teyvat.Mondstadt (mondstadt)
import Root.Teyvat.Liyue (liyue)
import Root.Teyvat.Inazuma (inazuma)
import Root.Teyvat.Snezhnaya (snezhnaya)

teyvat = clusterNode "Teyvat"
    World
    [

    ]
    [

    ]
    [
        mondstadt,
        liyue,
        inazuma,
        snezhnaya
    ]

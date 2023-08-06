{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.CruxFleet where
-- 南十字船队

import Irminsul
import Shortcut
import Root.Teyvat.Inazuma
import Root.Teyvat.Inazuma.Kaedehara (kazuhaKaedehara)

beidou = "Beidou"

cruxFleet = clusterLeaf "CruxFleet" Organization
    [
        beidou,
        kazuhaKaedehara
    ]
    [
    ]
    Nothing

{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.CruxFleet where
-- 南十字船队

import Irminsul
import Shortcut

beidou = "Beidou"

cruxFleet = clusterLeaf "CruxFleet" Organization
    [
        beidou
    ]
    [
    ]
    Nothing

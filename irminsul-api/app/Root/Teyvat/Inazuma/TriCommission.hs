module Root.Teyvat.Inazuma.TriCommission where

import Irminsul

import Root.Teyvat.Inazuma.TriCommission.YashiroCommission
import Root.Teyvat.Inazuma.TriCommission.TenryouCommission
import Root.Teyvat.Inazuma.TriCommission.KanjouCommission

triCommission = clusterNode "TriCommission" Organization
    [
        
    ]
    [

    ]
    [
        yashiroCommission,
        tenryouCommission,
        kanjouCommission
    ]
    (layout (0, 25) [
        cl tenryouCommission (0, 15) (0, 15) (12, 5),
        cl yashiroCommission (-17, -15) (-17, -15) (12, 5),
        cl kanjouCommission (17, -15) (17, -15) (12, 5)
    ])

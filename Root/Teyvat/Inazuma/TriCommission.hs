module Root.Teyvat.Inazuma.TriCommission where

import Irminsul
import Shortcut

import Root.Teyvat.Inazuma.TriCommission.YashiroCommission (yashiroCommission)
import Root.Teyvat.Inazuma.TriCommission.TenryouCommission (tenryouCommission)

triCommission = clusterNode "TriCommission" Organization
    [
        
    ]
    [

    ]
    [
        yashiroCommission,
        tenryouCommission
    ]

module Root.Teyvat.Inazuma.TriCommission where

import Irminsul
import Shortcut

import Root.Teyvat.Inazuma.TriCommission.YashiroCommission
import Root.Teyvat.Inazuma.TriCommission.TenryouCommission

triCommission = clusterNode "TriCommission" Organization
    [
        
    ]
    [

    ]
    [
        yashiroCommission,
        tenryouCommission
    ]
    Nothing

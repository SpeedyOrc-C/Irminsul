module Root.Teyvat.Inazuma.TriCommission where

import Irminsul

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

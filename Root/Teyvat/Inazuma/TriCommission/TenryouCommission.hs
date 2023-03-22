module Root.Teyvat.Inazuma.TriCommission.TenryouCommission where

import Irminsul
import Shortcut

saraKujo = ach "SaraKujo"

tenryouCommission = clusterLeaf "TenryouCommission" Organization
    [
        saraKujo
    ]
    [
    ]

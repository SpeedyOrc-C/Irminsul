module Root.Teyvat.Inazuma.TriCommission.TenryouCommission where

import Irminsul

saraKujo = "SaraKujo"
heizouShikanoin = "HeizouShikanoin"

tenryouCommission = clusterLeaf "TenryouCommission" Organization
    [
        saraKujo,
        heizouShikanoin
    ]
    [
    ]
    Nothing

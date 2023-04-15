module Root.Teyvat.Inazuma.TriCommission.TenryouCommission where

import Irminsul
import Shortcut

saraKujo = ach "SaraKujo"
heizouShikanoin = ach "HeizouShikanoin"

tenryouCommission = clusterLeaf "TenryouCommission" Organization
    [
        saraKujo,
        heizouShikanoin
    ]
    [
    ]
    Nothing

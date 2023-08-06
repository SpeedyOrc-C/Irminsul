{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.TriCommission.TenryouCommission where

import Irminsul
import Shortcut

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

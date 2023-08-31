{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.TriCommission.YashiroCommission where

import Irminsul
import CommonRelations

ayakaKamisato = "AyakaKamisato"
ayatoKamisato = "AyatoKamisato"

yashiroCommission = clusterLeaf "YashiroCommission" Organization
    [
        ayakaKamisato,
        ayatoKamisato
    ]
    [
        ayakaKamisato `youngerSister` ayatoKamisato,
        ayatoKamisato `elderBrother` ayakaKamisato
    ]
    (layout (0, 15) [
        al ayakaKamisato (-15, 0),
        al ayatoKamisato (15, 0)
    ])

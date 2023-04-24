{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.TriCommission.YashiroCommission where

import Irminsul
import Shortcut
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
    Nothing

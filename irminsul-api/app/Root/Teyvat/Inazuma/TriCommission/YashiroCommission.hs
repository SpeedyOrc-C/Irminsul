module Root.Teyvat.Inazuma.TriCommission.YashiroCommission where

import Irminsul
import CommonRelations

import Root.Teyvat.Mondstadt

ayakaKamisato = "AyakaKamisato"
ayatoKamisato = "AyatoKamisato"

yashiroCommission = clusterLeaf "YashiroCommission" Organization
    [
        ayakaKamisato,
        ayatoKamisato,
        thoma
    ]
    [
        ayakaKamisato `youngerSister` ayatoKamisato,
        ayatoKamisato `elderBrother` ayakaKamisato
    ]
    (layout (0, 15) [
        al ayakaKamisato (-7.5, 0),
        al ayatoKamisato (7.5, 0),
        al thoma (0, -15)
    ])

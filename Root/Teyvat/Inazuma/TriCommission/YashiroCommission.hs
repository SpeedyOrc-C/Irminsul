module Root.Teyvat.Inazuma.TriCommission.YashiroCommission where

import Irminsul
import CommonRelations

ayakaKamisato = ach "AyakaKamisato"
ayatoKamisato = ach "AyatoKamisato"

yashiroCommission = clusterLeaf "YashiroCommission" Organization
    [
        ayakaKamisato,
        ayatoKamisato
    ]
    [
        ayakaKamisato `youngerSister` ayatoKamisato,
        ayatoKamisato `elderBrother` ayakaKamisato
    ]

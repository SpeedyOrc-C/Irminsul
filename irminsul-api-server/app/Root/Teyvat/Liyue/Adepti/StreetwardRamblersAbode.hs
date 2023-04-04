module Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode where

import Irminsul
import Shortcut

streetwardRambler = ach "StreetwardRambler"
yaoyao = ach "Yaoyao"

streetwardRamblersAbode = clusterLeaf "StreetwardRamblersAbode" Organization
    [
        streetwardRambler,
        yaoyao
    ]
    [

    ]
    Nothing

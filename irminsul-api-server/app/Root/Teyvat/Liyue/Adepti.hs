module Root.Teyvat.Liyue.Adepti where
-- 仙人

import Irminsul
import Shortcut

import Root.Teyvat.Liyue.Adepti.Yaksha
import Root.Teyvat.Liyue.Adepti.SeaGazersAbode
import Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode
import Root.Teyvat.Liyue.Adepti.CloudRetainersAbode

adepti = clusterNode "Adepti" Organization
    [

    ]
    [

    ]
    [
        yaksha,
        cloudRetainersAbode,
        streetwardRamblersAbode,
        seaGazersAbode
    ]
    Nothing
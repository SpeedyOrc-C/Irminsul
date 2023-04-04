module Root.Teyvat.Liyue.Adepti where
-- 仙人

import Irminsul
import Shortcut
import Root.Teyvat.Liyue.Adepti.Yaksha (yaksha)
import Root.Teyvat.Liyue.Adepti.SeaGazersAbode (seaGazersAbode)
import Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode (streetwardRamblersAbode)
import Root.Teyvat.Liyue.Adepti.CloudRetainersAbode (cloudRetainersAbode)

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
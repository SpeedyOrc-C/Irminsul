module Root.Teyvat.Liyue.Adepti.CloudRetainersAbode where

import Shortcut
import Irminsul

cloudRetainer = ach "CloudRetainer"
shenhe = ach "Shenhe"

cloudRetainersAbode = clusterLeaf "CloudRetainersAbode" Organization
    [
        cloudRetainer,
        shenhe
    ]
    [

    ]
    Nothing

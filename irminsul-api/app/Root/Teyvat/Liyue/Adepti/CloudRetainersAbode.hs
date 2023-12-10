module Root.Teyvat.Liyue.Adepti.CloudRetainersAbode where

import Irminsul
import CommonRelations

cloudRetainer = "CloudRetainer"
ganyu = "Ganyu"
shenhe = "Shenhe"

cloudRetainersAbode = clusterLeaf "CloudRetainersAbode" Organization
    [
        cloudRetainer,
        ganyu,
        shenhe
    ]
    [
        ganyu `disciple` cloudRetainer,
        shenhe `disciple` cloudRetainer,
    
        ganyu `friend` shenhe
    ]
    (layout (0, 15) [
        al cloudRetainer (0, 0),
        al ganyu (-15, -15),
        al shenhe (15, -15)
    ])

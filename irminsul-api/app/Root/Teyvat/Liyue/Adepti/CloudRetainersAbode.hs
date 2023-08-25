{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti.CloudRetainersAbode where

import Shortcut
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
    (Just $ RelationGraphLayout {
        rootProperty=rl (0, 15),
        elementProperties=[
            al cloudRetainer (0, 0),
            al ganyu (-15, -15),
            al shenhe (15, -15)
        ]
    })

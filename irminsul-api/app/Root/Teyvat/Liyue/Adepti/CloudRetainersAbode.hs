{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti.CloudRetainersAbode where

import Shortcut
import Irminsul

cloudRetainer = "CloudRetainer"
ganyu = "Ganyu"
shenhe = "Shenhe"

cloudRetainersAbode = clusterLeaf "CloudRetainersAbode" Organization
    [
        cloudRetainer,
        shenhe,
        ganyu
    ]
    [

    ]
    Nothing

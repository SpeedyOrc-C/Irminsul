{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti.CloudRetainersAbode where

import Shortcut
import Irminsul

cloudRetainer = "CloudRetainer"
shenhe = "Shenhe"

cloudRetainersAbode = clusterLeaf "CloudRetainersAbode" Organization
    [
        cloudRetainer,
        shenhe
    ]
    [

    ]
    Nothing

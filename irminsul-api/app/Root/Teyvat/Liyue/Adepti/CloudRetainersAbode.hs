{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Adepti.CloudRetainersAbode where

import Shortcut
import Irminsul

cloudRetainer = "CloudRetainer"
ganyu = "Ganyu"

cloudRetainersAbode = clusterLeaf "CloudRetainersAbode" Organization
    [
        cloudRetainer,
        ganyu
    ]
    [

    ]
    Nothing

{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Kaeya where

import Irminsul
import Shortcut

import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.DawnWinery

kaeya = clusterLeaf "Kaeya" AnotherMe
    [
        kaeyaAlberich,
        kaeyaRagvindr
    ]
    [
    ]
    Nothing

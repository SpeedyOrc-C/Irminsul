{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.Qixing where
-- 七星

import Irminsul
import Shortcut

ningguang = "Ningguang"
keqing = "Keqing"

qixing = clusterLeaf "Qixing" Organization
    [
        ningguang,
        keqing
    ]
    [

    ]
    Nothing

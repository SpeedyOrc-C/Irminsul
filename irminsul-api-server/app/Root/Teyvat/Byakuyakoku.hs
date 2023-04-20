{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Byakuyakoku where

import Irminsul
import Shortcut

aberaku = "Aberaku"
adonis = "Adonis"
aru = "Aru"
clymene = "Clymene"
daimon = "Daimon"
date = "Date"
eboshi = "Eboshi"
eki = "Eki"
ema = "Ema"
supadaNoHiko = "SupadaNoHiko"
uda = "Uda"

tsumi = "Tsumi"

byakuyakoku = clusterLeaf "Byakuyakoku" Country
    [
        aberaku,
        adonis,
        aru,
        clymene,
        daimon,
        date,
        eboshi,
        eki,
        ema,
        supadaNoHiko,
        uda,
        tsumi
    ]
    [

    ]
    Nothing

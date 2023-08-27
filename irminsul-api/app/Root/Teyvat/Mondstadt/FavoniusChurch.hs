{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Mondstadt.FavoniusChurch where

import Irminsul
import Shortcut
import CommonRelations
import Root.Teyvat.Mondstadt.GunnhildrPegg

rosaria = "Rosaria"

favoniusChurch = clusterLeaf "FavoniusChurch"
    Organization
    [
        barbaraPegg,
        rosaria,
        simonPegg
    ]
    [
        
    ]
    (Just $ RelationGraphLayout {
        rootProperty=rl (0, 15),
        elementProperties=[
            al simonPegg (-15, 0),
            al barbaraPegg (0, 0),
            al rosaria (15, 0)
        ]
    })

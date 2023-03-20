module Root.Teyvat.Hexenzirkel where

import Irminsul
import Root.Teyvat.Khaenriah (rhinedottir)

-- TODO: Topmost atoms goes here


-- TODO: Topmost relations goes here


thisAtoms = [
    rhinedottir
    -- TODO: Put all topmost atoms into this list
    ]

thisRelations = [
    -- TODO: Put all topmost relations into this list
    ]

thisChildClusters = [
    -- TODO: Put all child clusters into this list
    ]

hexenzirkel = mergeFromChildClusters
    (Cluster "Hexenzirkel" Organization thisAtoms thisRelations)
    thisChildClusters

index = generateIndex thisChildClusters

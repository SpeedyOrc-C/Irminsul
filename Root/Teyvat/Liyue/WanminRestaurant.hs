module Root.Teyvat.Liyue.WanminRestaurant where
-- 万民堂

import Irminsul

-- TODO: Topmost atoms goes here
xiangling = ach "Xiangling"
guoba = ach "Guoba";
    marchosius = guoba

-- TODO: Topmost relations goes here


thisAtoms = [
    xiangling,
    guoba
    ]

thisRelations = [
    -- TODO: Put all topmost relations into this list
    ]

thisChildClusters = [
    -- TODO: Put all child clusters into this list
    ]

wanminRestaurant = mergeFromChildClusters
    (Cluster "WanminRestaurant" Organization thisAtoms thisRelations)
    thisChildClusters

index = generateIndex thisChildClusters

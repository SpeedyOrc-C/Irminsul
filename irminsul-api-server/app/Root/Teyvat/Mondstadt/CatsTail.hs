module Root.Teyvat.Mondstadt.CatsTail where

import Irminsul
import Shortcut
import CommonRelations

dionaKatzlein = ach "DionaKatzlein"
margaret = ach "Margaret"
shuyun = ach "Shuyun"
princeCat = ach "PrinceCat"
diane = ach "Diane"

catsTail = clusterLeaf "CatsTail" Organization
    [
        dionaKatzlein,
        margaret,
        shuyun,
        princeCat,
        diane
    ]
    [
        margaret `tavernOwner` catsTail,
        shuyun `translator` princeCat,
        princeCat `pet` margaret
    ]
    (Just $ RelationGraphLayout {
        rootProperty = rl (0, 15),
        elementProperties = [
            al dionaKatzlein (-15, -15),
            al margaret (-15, 0),
            al shuyun (15, 0),
            al princeCat (0, 0),
            al diane (15, -15)
        ]
    })

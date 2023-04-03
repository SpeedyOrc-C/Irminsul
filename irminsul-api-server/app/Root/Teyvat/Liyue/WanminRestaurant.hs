module Root.Teyvat.Liyue.WanminRestaurant where
-- 万民堂

import Irminsul
import Shortcut

xiangling = ach "Xiangling"
guoba = ach "Guoba";
    marchosius = guoba

wanminRestaurant = clusterLeaf "WanminRestaurant" Organization
    [
        xiangling,
        guoba
    ]
    [

    ]
    Nothing

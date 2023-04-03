module Root.Teyvat.Liyue.FeiyunCommerceGuild where
-- 飞云商会

import Irminsul
import Shortcut

xingqiu = ach "Xingqiu"

feiyunCommerceGuild = clusterLeaf "FeiyunCommerceGuild" Organization
    [
        xingqiu
    ]
    [

    ]
    Nothing

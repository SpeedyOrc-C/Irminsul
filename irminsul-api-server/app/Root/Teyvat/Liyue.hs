{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
import Shortcut
import Root.AnotherWorld (aether)
import Root.Teyvat.Liyue.Adepti (adepti)
import Root.Teyvat.Liyue.BubuPharmacy (bubuPharmary)
import Root.Teyvat.Liyue.CruxFleet (cruxFleet)
import Root.Teyvat.Liyue.FeiyunCommerceGuild (feiyunCommerceGuild)
import Root.Teyvat.Liyue.Qixing (qixing)
import Root.Teyvat.Liyue.WangshengFuneralParlor (wangshengFuneralParlor)
import Root.Teyvat.Liyue.WanminRestaurant (wanminRestaurant)
import Root.Teyvat.Liyue.YuehaiPavilion (yuehaiPavilion)
import Root.Teyvat.Liyue.YunHanOperaTroupe (yunHanOperaTroupe)

chongyun = ach "Chongyun"

xinyan = ach "Xinyan"

yelan = ach "Yelan"
yanfei = ach "Yanfei"

liyue = clusterNode "Liyue" Country
    [
        chongyun,
        xinyan,
        yelan,
        yanfei
    ]
    [

    ]
    [
        adepti,
        feiyunCommerceGuild,
        qixing,
        wangshengFuneralParlor,
        cruxFleet,
        wanminRestaurant,
        bubuPharmary,
        yuehaiPavilion,
        yunHanOperaTroupe
    ]
    Nothing

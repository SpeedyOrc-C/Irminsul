{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
import Root.Teyvat.Liyue.Adepti (adepti)
import Root.Teyvat.Liyue.FeiyunCommerceGuild (feiyunCommerceGuild)
import Root.Teyvat.Liyue.Qixing (qixing)
import Root.Teyvat.Liyue.WangshengFuneralParlor (wangshengFuneralParlor)
import Root.Teyvat.Liyue.CruxFleet (cruxFleet)

chongyun = ach "Chongyun"
yunJin = ach "YunJin"
xinyan = ach "Xinyan"
qiqi = ach "Qiqi"
ganyu = ach "Ganyu"
yelan = ach "Yelan"
yaoyao = ach "Yaoyao"
yanfei = ach "Yanfei"

liyue = clusterNode "Liyue" Country
    [
        chongyun,
        yunJin,
        xinyan,
        qiqi,
        ganyu,
        yelan,
        yaoyao,
        yanfei
    ]
    [

    ]
    [
        adepti,
        feiyunCommerceGuild,
        qixing,
        wangshengFuneralParlor,
        cruxFleet
    ]

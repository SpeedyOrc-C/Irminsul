{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
import Shortcut
import CommonRelations

import Root.AnotherWorld
import Root.Teyvat.Liyue.Adepti
import Root.Teyvat.Liyue.BubuPharmacy
import Root.Teyvat.Liyue.CruxFleet
import Root.Teyvat.Liyue.FeiyunCommerceGuild
import Root.Teyvat.Liyue.Qixing
import Root.Teyvat.Liyue.WangshengFuneralParlor
import Root.Teyvat.Liyue.WanminRestaurant
import Root.Teyvat.Liyue.YuehaiPavilion
import Root.Teyvat.Liyue.YunHanOperaTroupe

chongyun = "Chongyun"

xinyan = "Xinyan"

yelan = "Yelan"
yanfei = "Yanfei"

liyue = clusterNode "Liyue" Country
    [
        chongyun,
        xinyan,
        yelan,
        yanfei
    ]
    [
        ningguang `friend` beidou,
        ningguang `friend` yelan,
        ningguang `appreciate` yunJin
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

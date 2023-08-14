{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
import Shortcut
import CommonRelations

import Root.Teyvat.Liyue.Adepti
import Root.Teyvat.Liyue.BubuPharmacy
import Root.Teyvat.Liyue.CruxFleet
import Root.Teyvat.Liyue.FeiyunCommerceGuild
import Root.Teyvat.Liyue.Qixing
import Root.Teyvat.Liyue.WangshengFuneralParlor
import Root.Teyvat.Liyue.WanminRestaurant
import Root.Teyvat.Liyue.YuehaiPavilion
import Root.Teyvat.Liyue.YunHanOperaTroupe
import Root.Teyvat.Liyue.QingceVillage
import Root.Teyvat.Liyue.Adepti.Yaksha
import Root.Teyvat.Liyue.Adepti.CloudRetainersAbode

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
        zhongli `rule` liyue,
        zhongli `appreciate` keqing,
        zhongli `appreciate` ningguang,
        zhongli `lookAfter` xiao,
        
        ningguang `friend` beidou,
        ningguang `friend` yelan,
        yelan `intelligenceOfficer` ningguang,
        ningguang `appreciate` jinYun,

        beidou `appreciate` xiangling,
        xiangling `cook` cruxFleet,

        qiqi `afraidOf` hutao,

        xingqiu `friend` chongyun
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
        yunHanOperaTroupe,
        qingceVillage
    ]
    (Just $ RelationGraphLayout {
        rootProperty=rl (-15, 15),
        elementProperties=[
            al zhongli (-15, 0),
            al ningguang (0, 0),
            al jinYun (0, -15),
            al yelan (0, 15),
            al beidou (15, 0),
            al keqing (-15, -15),
            al xiao (-30, 0),
            al hutao (-30, -15),
            al qiqi (-45, -15),
            al baizhu (-45, 0),
            al xiangling (15, -15),
            al chongyun (30, 15),
            al xingqiu (15, 15),
            al shenhe (45, 15),
            al xinyan (0, -30),

            cl cruxFleet (35, 0) (35, 0) (15, 5)
        ]    
    })

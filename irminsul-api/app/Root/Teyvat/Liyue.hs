module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
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
import Root.AnotherWorld
import Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode
import Root.Teyvat.Inazuma.Kaedehara

chongyun = "Chongyun"

xinyan = "Xinyan"

yelan = "Yelan"
yanfei = "Yanfei"

liyue = clusterNodeWithExclusion "Liyue" Country
    ([
        chongyun,
        xinyan,
        yelan,
        yanfei,
        shenhe,
        travellerArchonSide
    ], [
        kazuhaKaedehara
    ])
    [
        zhongli `rule` liyue,
        zhongli `appreciate` keqing,
        zhongli `appreciate` ningguang,
        zhongli `lookAfter` xiao,

        ningguang `friend` beidou,
        ningguang `notSatisfiedWith` beidou,
        ningguang `friend` yelan,
        yelan `intelligenceOfficer` ningguang,
        ningguang `appreciate` jinYun,

        beidou `appreciate` xiangling,
        Relation "HelpTasteDishes" beidou xiangling,
        xiangling `cook` cruxFleet,

        qiqi `afraidOf` hutao,

        xingqiu `friend` chongyun,

        xinyan `friend` xiangling,
        xinyan `friend` jinYun,
        jinYun `appreciate` xinyan,
        keqing `intrestedIn` xinyan,

        Relation "Smuggle" beidou travellerArchonSide,

        Relation "YoungerSisterOfMother" shenhe chongyun,

        yanfei `friend` ganyu,
        yanfei `rival` ningguang,
        yanfei `cooperate` xingqiu
    ]
    [
        adepti,
        feiyunCommerceGuild,
        qixing,
        wangshengFuneralParlor,
        cruxFleet,
        wanminRestaurant,
        bubuPharmacy,
        yuehaiPavilion,
        yunHanOperaTroupe,
        qingceVillage
    ]
    (layout (-15, 15) [
        al zhongli (-15, 0),
        al ningguang (0, 0),
        al jinYun (0, -15),
        al yelan (0, 15),
        al beidou (15, -10.5),
        al keqing (-15, -15),
        al xiao (-30, 0),
        al hutao (-30, -15),
        al qiqi (-45, -15),
        al baizhu (-45, 0),
        al xiangling (15, -30),
        al chongyun (30, 15),
        al xingqiu (15, 15),
        al shenhe (45, 15),
        al xinyan (0, -30),
        al travellerArchonSide (30, -15),
        al yaoyao (45, 0),
        al ganyu (30, 0),
        al yanfei (15, 0),

        cl cruxFleet (35, -30) (30, -30) (15, 5),
        cl wangshengFuneralParlor (-30, -22.5) (-30, -22.5) (12, 5),
        cl bubuPharmacy (-45, -7.5) (-45, -7.5) (12, 5),
        cl adepti (-35, 15) (-35, 15) (12, 5)
    ])

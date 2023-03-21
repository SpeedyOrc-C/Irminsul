{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
import Root.Teyvat.Liyue.Adepti (adepti)

zhongli = ach "Zhongli";
    morax = zhongli

xingqiu = ach "Xingqiu"
chongyun = ach "Chongyun"
yunJin = ach "YunJin"
xinyan = ach "Xinyan"
ningguang = ach "Ningguang"
keqing = ach "Keqing"
qiqi = ach "Qiqi"
hutao = ach "Hutao"
ganyu = ach "Ganyu"
yelan = ach "Yelan"
beidou = ach "Beidou"
yaoyao = ach "Yaoyao"
yanfei = ach "Yanfei"

liyue = clusterNode "Liyue" Country
    [
        morax,
        xingqiu,
        chongyun,
        yunJin,
        xinyan,
        ningguang,
        keqing,
        qiqi,
        hutao,
        ganyu,
        yelan,
        beidou,
        yaoyao,
        yanfei
    ]
    [

    ]
    [
        adepti
    ]

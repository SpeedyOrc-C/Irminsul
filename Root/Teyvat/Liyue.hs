{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Liyue where
-- 璃月

import Irminsul
import Root.Teyvat.Liyue.Adepti (adepti)

zhongli = Atom "Zhongli" Character;
    morax = zhongli

xingqiu = Atom "Xingqiu" Character
chongyun = Atom "Chongyun" Character
yunJin = Atom "YunJin" Character
xinyan = Atom "Xinyan" Character
ningguang = Atom "Ningguang" Character
keqing = Atom "Keqing" Character
qiqi = Atom "Qiqi" Character
hutao = Atom "Hutao" Character
ganyu = Atom "Ganyu" Character
yelan = Atom "Yelan" Character
beidou = Atom "Beidou" Character
yaoyao = Atom "Yaoyao" Character
yanfei = Atom "Yanfei" Character

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

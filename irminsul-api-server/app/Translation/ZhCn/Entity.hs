module Translation.ZhCn.Entity where

import Data.Maybe

import Irminsul
import Shortcut
import Prelude hiding ((.))
import Translation.ZhCn.Utility

import Root
import Root.AnotherWorld
import Root.Teyvat
import Root.Teyvat.Mondstadt
import Root.Teyvat.Mondstadt.CatsTail
import Root.Teyvat.Mondstadt.DawnWinery
import Root.Teyvat.Mondstadt.FavoniusChurch
import Root.Teyvat.Mondstadt.Lupical
import Root.Teyvat.Mondstadt.KnightsOfFavonius
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam
import Root.Teyvat.Kaeya
import Root.Teyvat.Khaenriah
import Root.Teyvat.Hexenzirkel
import Root.Teyvat.Fontaine
import Root.Teyvat.Mondstadt.TheFourWinds
import Root.Teyvat.Liyue
import Root.Teyvat.Inazuma
import Root.Teyvat.Liyue.WangshengFuneralParlor


translationEntityZhCn :: [(Entity, Information)]
translationEntityZhCn = [
    ipn aether "空",
    ip albedo "阿贝多" ["阿贝多先生", "白垩之子"] UntilNow "",
    ipn alice "爱丽丝",
    ip amber "安柏" ["打火姬", "蒙德飞行冠军"] UntilNow "",
    ipn barbaraPegg ("芭芭拉"."佩奇"),
    ipn bennett "班尼特",
    ipn boreas "玻瑞亚斯",
    ipn catsTail "猫尾酒馆",
    ipn dawnWinery "晨曦酒庄",
    ipn diane "黛安",
    ipn dilucRagvindr ("迪卢克"."莱艮芬德"),
    ipn dionaKatzlein "迪奥娜",
    ipn dvalin "特瓦林",
    ip eulaLawrence ("优菈"."劳伦斯") ["浪花骑士", "喷嚏记仇真君"] UntilNow "",
    ipn favoniusChurch "西风教会",
    ipn fischlVonLuftschlossNarfidort ("菲谢尔"."冯"."露弗施洛斯"."那菲多特"),
    ipn guerillaTeam "游击小队",
    ipn inazuma "稻妻",
    ip jeanGunnhildr ("琴"."古恩希尔德") ["琴妈", "骑士团的魂"] UntilNow "",
    ip kaeya "凯亚" ["矿工头子"] UntilNow "",
    ipn kaeyaAlberich ("凯亚"."亚尔伯里奇"),
    ipn kaeyaRagvindr ("凯亚"."莱艮芬德"),
    ip klee "可莉" ["蒙德最强战力", "火花骑士"] UntilNow "",
    ipn knightsOfFavonius "西风骑士团",
    ip lisa "丽莎" ["丽莎阿姨", "蒙德雷神"] UntilNow "",
    ipn liyue "璃月",
    ipn lumine "荧",
    ipn margaret "玛格丽特",
    ip mikaSchmidt ("米卡"."施密特") ["前进测绘员"] UntilNow "",
    ipn monaAstrologistMegistus ("阿斯托洛吉斯"."莫娜"."梅姬斯图斯"),
    ipn mondstadt "蒙德",
    ip noelle "诺艾尔" ["女仆", "高达", "岩王帝姬"] UntilNow "",
    ipn ozvaldoHrafnavins ("奥兹华尔多"."赫芙那梵茵斯"),
    ipn princeCat "小王子",
    ipn razor "雷泽",
    ipn rosaria "罗莎莉亚",
    ipn root "树根",
    ipn shuyun "恕筠",
    ip sucrose "砂糖" ["雷萤术士"] UntilNow "",
    ipn teyvat "提瓦特",
    ipn theFourWinds "四风守护",
    ipn varka "法尔伽",
    ipn venti "温迪",
    ipn zhongli "钟离"
    ]

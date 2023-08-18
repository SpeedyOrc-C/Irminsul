module Translation.ZhCn.Entity where

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
import Root.Teyvat.Fontaine.TheSteambird
import Root.Teyvat.Liyue.QingceVillage
import Root.Teyvat.Liyue.WanminRestaurant
import Root.Teyvat.Liyue.FeiyunCommerceGuild
import Root.Teyvat.Liyue.Qixing
import Root.Teyvat.Byakuyakoku
import Root.Teyvat.Mondstadt.Gunnhildr
import Root.Teyvat.Liyue.CruxFleet
import Root.Teyvat.Liyue.Adepti.Yaksha
import Root.Teyvat.Liyue.BubuPharmacy
import Root.Teyvat.Inazuma.Kaedehara
import Root.Teyvat.Liyue.YunHanOperaTroupe
import Root.Teyvat.Liyue.Adepti.StreetwardRamblersAbode
import Root.Teyvat.Liyue.Adepti.CloudRetainersAbode

translationEntityZhCn :: [(Entity, Information)]
translationEntityZhCn = [
    ipn aberaku "阿倍良久",
    ipn adonis "阿多尼斯",
    ipn aether "空",
    ip albedo "阿贝多" ["阿贝多先生", "白垩之子"] UntilNow "",
    ipn alice "爱丽丝",
    ip amber "安柏" ["打火姬", "蒙德飞行冠军"] UntilNow "",
    ipn aru "亚琉",
    ipn barbaraPegg ("芭芭拉"."佩奇"),
    ipn baizhu "白术",
    ipn beidou "北斗",
    ipn bennett "班尼特",
    ipn boreas "玻瑞亚斯",
    ipn bubuPharmary "不卜庐",
    ipn catsTail "猫尾酒馆",
    ipn charlotte "夏洛蒂",
    ipn chongyun "重云",
    ipn cloudRetainersAbode "留云借风真君洞天",
    ipn cruxFleet "南十字船队",
    ipn daimon "大门",
    ipn dawnWinery "晨曦酒庄",
    ipn diane "黛安",
    ipn dilucRagvindr ("迪卢克"."莱艮芬德"),
    ipn dionaKatzlein "迪奥娜",
    ipn dvalin "特瓦林",
    ipn eboshi "乌帽子",
    ipn ema "绘真",
    ip eulaLawrence ("优菈"."劳伦斯") ["浪花骑士", "喷嚏记仇真君"] UntilNow "",
    ipn favoniusChurch "西风教会",
    ipn fischlVonLuftschlossNarfidort ("菲谢尔"."冯"."露弗施洛斯"."那菲多特"),
    ipn ganyu "甘雨",
    ipn guerillaTeam "游击小队",
    ipn hutao "胡桃",
    ipn inazuma "稻妻",
    ip jeanGunnhildr ("琴"."古恩希尔德") ["琴妈", "骑士团的魂"] UntilNow "",
    ipn jinYun "云堇",
    ip kaeya "凯亚" ["矿工头子"] UntilNow "",
    ipn kaeyaAlberich ("凯亚"."亚尔伯里奇"),
    ipn kaeyaRagvindr ("凯亚"."莱艮芬德"),
    ipn kazuhaKaedehara "枫原万叶",
    ipn keqing "刻晴",
    ipn kirara "绮良良",
    ip klee "可莉" ["蒙德最强战力", "火花骑士"] UntilNow "",
    ipn knightsOfFavonius "西风骑士团",
    ip lisa "丽莎" ["丽莎阿姨", "蒙德雷神"] UntilNow "",
    ipn liyue "璃月",
    ipn lumine "荧",
    ipn margaret "玛格丽特",
    ip mikaSchmidt ("米卡"."施密特") ["前进测绘员"] UntilNow "",
    ipn monaAstrologistMegistus ("阿斯托洛吉斯"."莫娜"."梅姬斯图斯"),
    ipn mondstadt "蒙德",
    ipn ningguang "凝光",
    ip noelle "诺艾尔" ["女仆", "高达", "岩王帝姬"] UntilNow "",
    ipn ozvaldoHrafnavins ("奥兹华尔多"."赫芙那梵茵斯"),
    ipn princeCat "小王子",
    ipn qingceVillage "轻策庄",
    ipn qiqi "七七",
    ipn razor "雷泽",
    ipn rosaria "罗莎莉亚",
    ipn root "树根",
    ipn shenhe "申鹤",
    ipn shuyun "恕筠",
    ip sucrose "砂糖" ["雷萤术士"] UntilNow "",
    ipn supadaNoHiko "须婆达之彦",
    ipn teyvat "提瓦特",
    ipn theFourWinds "四风守护",
    ipn uda "宇陀",
    ipn varka "法尔伽",
    ipn venti "温迪",
    ipn wangshengFuneralParlor "往生堂",
    ipn xiangling "香菱",
    ipn xiao "魈",
    ipn xinyan "辛焱",
    ipn xingqiu "行秋",
    ipn yaoyao "瑶瑶",
    ipn yelan "夜兰",
    ipn zhongli "钟离"
    ]

module Translation.ZhCn.Entity where

import Data.Maybe

import Irminsul
import Shortcut
import Translation.ZhCn.Utility

import Root.AnotherWorld
import Root.Teyvat.Mondstadt.KnightsOfFavonius
import Root.Teyvat.Kaeya
import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.DawnWinery
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam

translationEntityZhCn :: [(Entity, Information)]
translationEntityZhCn = [
    ip knightsOfFavonius "西风骑士团" [] UntilNow "",
        ip jeanGunnhildr ("琴".="古恩希尔德") ["琴妈", "骑士团的魂"] UntilNow "",
        ip lisa "丽莎" ["丽莎阿姨", "蒙德雷神"] UntilNow "",
        ip kaeya "凯亚" ["矿工头子"] UntilNow "",
            ip kaeyaAlberich ("凯亚".="亚尔伯里奇") [] UntilNow "",
            ip kaeyaRagvindr ("凯亚".="莱艮芬德") [] UntilNow "",
        ip amber "安柏" ["打火姬", "蒙德飞行冠军"] UntilNow "",
        ip klee "可莉" ["蒙德最强战力", "火花骑士"] UntilNow "",
        ip albedo "阿贝多" ["阿贝多先生", "白垩之子"] UntilNow "",
        ip sucrose "砂糖" ["雷萤术士"] UntilNow "",
        ip noelle "诺艾尔" ["女仆", "高达", "岩王帝姬"] UntilNow "",
    
    ip guerillaTeam "游击小队" [] UntilNow "",
        ip eulaLaurence ("优菈".="劳伦斯") ["浪花骑士", "喷嚏记仇真君"] UntilNow "",
        ip mikaSchmidt ("米卡".="施密特") ["前进测绘员"] UntilNow "",
    
    ip aether "空" [] UntilNow "",
    ip lumine "荧" [] UntilNow ""
    ]

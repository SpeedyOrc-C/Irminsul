module Translation.EnUs.Entity where

import Data.Maybe

import Irminsul
import Shortcut

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
import Root.Teyvat.Liyue.YuehaiPavilion
import Root.Teyvat.Byakuyakoku

translationEntityEnUs :: [(Entity, Information)]
translationEntityEnUs = [
    ipn aberaku "Aberaku",
    ipn adonis "Adonis",
    ipn aether "Aether",
    ipn albedo "Albedo",
    ipn alice "Alice",
    ipn amber "Amber",
    ipn aru "Aru",
    ipn barbaraPegg "Barbara Pegg",
    ipn bennett "Bennett",
    ipn boreas "Boreas",
    ipn catsTail "Cat's Tail",
    ipn charlotte "Charlotte",
    ipn chongyun "Chongyun",
    ipn daimon "Daimon",
    ipn dawnWinery "Dawn Winery",
    ipn diane "Diane",
    ipn dilucRagvindr "Diluc Ragvindr",
    ipn dionaKatzlein "Diona Katzlein",
    ipn dvalin "Dvalin",
    ipn eboshi "Eboshi",
    ipn ema "Ema",
    ipn eulaLawrence "Eula Lawrence",
    ipn favoniusChurch "Favonius Church",
    ipn fischlVonLuftschlossNarfidort "Fischl von Luftschloss Narfidort",
    ipn ganyu "Ganyu",
    ipn guerillaTeam "Guerilla Team",
    ipn inazuma "Inazuma",
    ipn jeanGunnhildr "Jean Gunnhildr",
    ipn kaeya "Kaeya",
    ipn kaeyaAlberich "Kaeya Alberich",
    ipn kaeyaRagvindr "Kaeya Ragvindr",
    ipn keqing "Keqing",
    ipn kirara "Kirara",
    ipn klee "Klee",
    ipn knightsOfFavonius "Knights of Favonius",
    ipn lisa "Lisa",
    ipn liyue "Liyue",
    ipn lumine "Lumine",
    ipn mikaSchmidt "Mika Schmidt",
    ipn margaret "Margaret",
    ipn monaAstrologistMegistus "Mona Megistus, the Astrologist",
    ipn mondstadt "Mondstadt",
    ipn ozvaldoHrafnavins "Ozvaldo Hrafnavins",
    ipn princeCat "Prince",
    ipn noelle "Noelle",
    ipn qingceVillage "Qingce Village",
    ipn razor "Razor",
    ipn rosaria "Rosaria",
    ipn root "Root",
    ipn shuyun "Shuyun",
    ipn sucrose "Sucrose",
    ipn supadaNoHiko "Supada no Hiko",
    ipn teyvat "Teyvat",
    ipn theFourWinds "The Four Winds",
    ipn uda "Uda",
    ipn varka "Varka",
    ipn venti "Venti",
    ipn xiangling "Xiangling",
    ipn xingqiu "Xingqiu",
    ipn zhongli "Zhongli"
    ]

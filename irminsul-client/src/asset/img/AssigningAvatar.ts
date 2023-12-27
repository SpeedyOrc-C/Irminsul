import Unknown from "./UnknownAvatar.png";

import Aether from "./avatar/Aether.png";
import Albedo from "./avatar/Albedo.png";
import Alhaitham from "./avatar/Alhaitham.png";
import Amber from "./avatar/Amber.png";
import AyakaKamisato from "./avatar/AyakaKamisato.png";
import AyatoKamisato from "./avatar/AyatoKamisato.png";
import Baizhu from "./avatar/Baizhu.png";
import BarbaraPegg from "./avatar/BarbaraPegg.png";
import Beidou from "./avatar/Beidou.png";
import Bennett from "./avatar/Bennett.png";
import Candace from "./avatar/Candace.png";
import Charlotte from "./avatar/Charlotte.png";
import Chevreuse from "./avatar/Chevreuse.png";
import Chongyun from "./avatar/Chongyun.png";
import Collei from "./avatar/Collei.png";
import Cyno from "./avatar/Cyno.png";
import Dehya from "./avatar/Dehya.png";
import Diane from "./avatar/Diane.png";
import DilucRagvindr from "./avatar/DilucRagvindr.png";
import DionaKatzlein from "./avatar/DionaKatzlein.png";
import Dori from "./avatar/Dori.png";
import Dvalin from "./avatar/Dvalin.png";
import EiRaiden from "./avatar/EiRaiden.png";
import EulaLawrence from "./avatar/EulaLawrence.png";
import Faruzan from "./avatar/Faruzan.png";
import FischlVonLuftschlossNarfidort from "./avatar/FischlVonLuftschlossNarfidort.png";
import Freminet from "./avatar/Freminet.png";
import Furina from "./avatar/Furina.png";
import Ganyu from "./avatar/Ganyu.png";
import Gorou from "./avatar/Gorou.png";
import HeizouShikanoin from "./avatar/HeizouShikanoin.png";
import Hutao from "./avatar/Hutao.png";
import IttoArataki from "./avatar/IttoArataki.png";
import JeanGunnhildr from "./avatar/JeanGunnhildr.png";
import JinYun from "./avatar/JinYun.png";
import KaeyaAlberich from "./avatar/KaeyaAlberich.png";
import KaeyaRagvindr from "./avatar/KaeyaRagvindr.png";
import Kaveh from "./avatar/Kaveh.png";
import KazuhaKaedehara from "./avatar/KazuhaKaedehara.png";
import Keqing from "./avatar/Keqing.png";
import Klee from "./avatar/Klee.png";
import KokomiSangonomiya from "./avatar/KokomiSangonomiya.png";
import Layla from "./avatar/Layla.png";
import Lisa from "./avatar/Lisa.png";
import Lumine from "./avatar/Lumine.png";
import Lynette from "./avatar/Lynette.png";
import Lyney from "./avatar/Lyney.png";
import MikaSchmidt from "./avatar/MikaSchmidt.png";
import MikoYae from "./avatar/MikoYae.png";
import MonaAstrologistMegistus from "./avatar/MonaAstrologistMegistus.png";
import Nahida from "./avatar/Nahida.png";
import Navia from "./avatar/Navia.png";
import Neuvillette from "./avatar/Neuvillette.png";
import Nilou from "./avatar/Nilou.png";
import Ningguang from "./avatar/Ningguang.png";
import Noelle from "./avatar/Noelle.png";
import Orobaxi from "./avatar/Orobaxi.png";
import OzvaldoHrafnavins from "./avatar/OzvaldoHrafnavins.png";
import Paimon from "./avatar/Paimon.png";
import PrinceCat from "./avatar/PrinceCat.png";
import Qiqi from "./avatar/Qiqi.png";
import Razor from "./avatar/Razor.png";
import Rosaria from "./avatar/Rosaria.png";
import RukkhadevataGreaterLord from "./avatar/RukkhadevataGreaterLord.png";
import SaraKujo from "./avatar/SaraKujo.png";
import Sayu from "./avatar/Sayu.png";
import Shenhe from "./avatar/Shenhe.png";
import ShinobuKuki from "./avatar/ShinobuKuki.png";
import Sucrose from "./avatar/Sucrose.png";
import Tartaglia from "./avatar/Tartaglia.png";
import Thoma from "./avatar/Thoma.png";
import Tighnari from "./avatar/Tighnari.png";
import Venti from "./avatar/Venti.png";
import Wanderer from "./avatar/Wanderer.png";
import Wriothesley from "./avatar/Wriothesley.png";
import Xiangling from "./avatar/Xiangling.png";
import Xiao from "./avatar/Xiao.png";
import Xingqiu from "./avatar/Xingqiu.png";
import Xinyan from "./avatar/Xinyan.png";
import Yanfei from "./avatar/Yanfei.png";
import Yaoyao from "./avatar/Yaoyao.png";
import Yelan from "./avatar/Yelan.png";
import YoimiyaNaganohara from "./avatar/YoimiyaNaganohara.png";
import Zhongli from "./avatar/Zhongli.png";

const assigner = new Map<string, string>([
    ["Aether", Aether],
    ["Albedo", Albedo],
    ["Alhaitham", Alhaitham],
    ["Amber", Amber],
    ["AyakaKamisato", AyakaKamisato],
    ["AyatoKamisato", AyatoKamisato],
    ["Baizhu", Baizhu],
    ["BarbaraPegg", BarbaraPegg],
    ["Beidou", Beidou],
    ["Bennett", Bennett],
    ["Candace", Candace],
    ["Charlotte", Charlotte],
    ["Chevreuse", Chevreuse],
    ["Chongyun", Chongyun],
    ["Collei", Collei],
    ["Cyno", Cyno],
    ["Dehya", Dehya],
    ["Diane", Diane],
    ["DilucRagvindr", DilucRagvindr],
    ["DionaKatzlein", DionaKatzlein],
    ["Dori", Dori],
    ["Dvalin", Dvalin],
    ["EiRaiden", EiRaiden],
    ["EulaLawrence", EulaLawrence],
    ["Faruzan", Faruzan],
    ["FischlVonLuftschlossNarfidort", FischlVonLuftschlossNarfidort],
    ["Freminet", Freminet],
    ["Furina", Furina],
    ["Ganyu", Ganyu],
    ["Gorou", Gorou],
    ["HeizouShikanoin", HeizouShikanoin],
    ["Hutao", Hutao],
    ["IttoArataki", IttoArataki],
    ["JeanGunnhildr", JeanGunnhildr],
    ["JinYun", JinYun],
    ["KaeyaAlberich", KaeyaAlberich],
    ["KaeyaRagvindr", KaeyaRagvindr],
    ["Kaveh", Kaveh],
    ["KazuhaKaedehara", KazuhaKaedehara],
    ["Keqing", Keqing],
    ["Klee", Klee],
    ["KokomiSangonomiya", KokomiSangonomiya],
    ["Layla", Layla],
    ["Lisa", Lisa],
    ["Lumine", Lumine],
    ["Lynette", Lynette],
    ["Lyney", Lyney],
    ["MikaSchmidt", MikaSchmidt],
    ["MikoYae", MikoYae],
    ["MonaAstrologistMegistus", MonaAstrologistMegistus],
    ["Nahida", Nahida],
    ["Navia", Navia],
    ["Neuvillette", Neuvillette],
    ["Nilou", Nilou],
    ["Ningguang", Ningguang],
    ["Noelle", Noelle],
    ["Orobaxi", Orobaxi],
    ["OzvaldoHrafnavins", OzvaldoHrafnavins],
    ["Paimon", Paimon],
    ["PrinceCat", PrinceCat],
    ["Qiqi", Qiqi],
    ["Razor", Razor],
    ["Rosaria", Rosaria],
    ["RukkhadevataGreaterLord", RukkhadevataGreaterLord],
    ["SaraKujo", SaraKujo],
    ["Sayu", Sayu],
    ["Shenhe", Shenhe],
    ["ShinobuKuki", ShinobuKuki],
    ["Sucrose", Sucrose],
    ["Tartaglia", Tartaglia],
    ["Thoma", Thoma],
    ["Tighnari", Tighnari],
    ["Venti", Venti],
    ["Wanderer", Wanderer],
    ["Wriothesley", Wriothesley],
    ["Xiangling", Xiangling],
    ["Xiao", Xiao],
    ["Xingqiu", Xingqiu],
    ["Xinyan", Xinyan],
    ["Yanfei", Yanfei],
    ["Yaoyao", Yaoyao],
    ["Yelan", Yelan],
    ["YoimiyaNaganohara", YoimiyaNaganohara],
    ["Zhongli", Zhongli],
]);

export function getAvatar(id: string): string {
    return assigner.get(id) ?? Unknown;
}

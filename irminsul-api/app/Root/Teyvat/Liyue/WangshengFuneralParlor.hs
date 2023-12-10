module Root.Teyvat.Liyue.WangshengFuneralParlor where
-- 往生堂

import Irminsul
import CommonRelations

zhongli = "Zhongli";
    morax = zhongli
hutao = "Hutao"

wangshengFuneralParlor = clusterLeaf "WangshengFuneralParlor" Organization
    [
        zhongli,
        hutao
    ]
    [
        hutao `superior` zhongli
    ]
    Nothing

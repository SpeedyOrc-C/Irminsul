module Root.Teyvat.Liyue.WangshengFuneralParlor where
-- 往生堂

import Irminsul
import Shortcut

zhongli = ach "Zhongli";
    morax = zhongli
hutao = ach "Hutao"

wangshengFuneralParlor = clusterLeaf "WangshengFuneralParlor" Organization
    [
        zhongli,
        hutao
    ]
    [
        
    ]
    Nothing

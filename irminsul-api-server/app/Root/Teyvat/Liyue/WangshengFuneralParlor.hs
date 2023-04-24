{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.WangshengFuneralParlor where
-- 往生堂

import Irminsul
import Shortcut

zhongli = "Zhongli";
    morax = zhongli
hutao = "Hutao"

wangshengFuneralParlor = clusterLeaf "WangshengFuneralParlor" Organization
    [
        zhongli,
        hutao
    ]
    [
        
    ]
    Nothing

{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.BubuPharmacy where
-- 不卜庐

import Irminsul
import Shortcut

qiqi = "Qiqi"
baizhu = "Baizhu"

bubuPharmacy = clusterLeaf "BubuPharmacy" Organization
    [
        qiqi,
        baizhu
    ]
    [
        
    ]
    Nothing

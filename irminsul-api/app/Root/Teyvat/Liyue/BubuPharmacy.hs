{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.BubuPharmacy where
-- 不卜庐

import Irminsul

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

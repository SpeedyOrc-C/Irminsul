module Root.Teyvat.Liyue.BubuPharmacy where
-- 不卜庐

import Irminsul
import Shortcut

qiqi = ach "Qiqi"
baizhu = ach "Baizhu"

bubuPharmary = clusterLeaf "BubuPharmacy" Organization
    [
        qiqi,
        baizhu
    ]
    [
        
    ]
    Nothing

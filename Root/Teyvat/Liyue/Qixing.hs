module Root.Teyvat.Liyue.Qixing where
-- 七星

import Irminsul
import Shortcut

ningguang = ach "Ningguang"
keqing = ach "Keqing"

qixing = clusterLeaf "Qixing" Organization
    [
        ningguang,
        keqing
    ]
    [

    ]

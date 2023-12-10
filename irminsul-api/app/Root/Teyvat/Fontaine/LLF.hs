module Root.Teyvat.Fontaine.LLF where

import Irminsul
import CommonRelations

lyney = "Lyney"
lynette = "Lynette"
freminet = "Freminet"

llf = clusterLeaf "LLF" Family
    [
        lyney,
        lynette,
        freminet   
    ]
    [
        lyney `elderBrother` lynette,
        lynette `youngerSister` lyney
    ]
    Nothing

module Root.Teyvat.Sumeru.Akademiya.Haravatat where

import Irminsul
import CommonRelations

import Root.Teyvat.Sumeru.GrandharvaVille

faruzan = "Faruzan"
alhaitham = "Alhaitham"

haravatat = clusterLeaf "Haravatat" Organization
    [
        faruzan,
        alhaitham
    ]
    [
        faruzan `teacher` collei
    ]
    Nothing

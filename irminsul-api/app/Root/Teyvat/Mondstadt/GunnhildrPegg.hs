module Root.Teyvat.Mondstadt.GunnhildrPegg where

import Irminsul
import CommonRelations


fredericaGunnhildr = "FredericaGunnhildr"
jeanGunnhildr = "JeanGunnhildr"
simonPegg = "SimonPegg"
barbaraPegg = "BarbaraPegg"

gunnhildrPegg = clusterLeaf "GunnhildrPegg" Family
    [
        fredericaGunnhildr,
        simonPegg,
        jeanGunnhildr,
        barbaraPegg
    ]
    [
        fredericaGunnhildr `mother` jeanGunnhildr,
        jeanGunnhildr `daughter` fredericaGunnhildr,

        fredericaGunnhildr `mother` barbaraPegg,
        barbaraPegg `daughter` fredericaGunnhildr,

        simonPegg `father` jeanGunnhildr,
        jeanGunnhildr `daughter` simonPegg,

        simonPegg `father` barbaraPegg,
        barbaraPegg `daughter` simonPegg,

        barbaraPegg `youngerSister` jeanGunnhildr,
        jeanGunnhildr `elderSister` barbaraPegg
    ]
    (layout (0, 15) [
        al fredericaGunnhildr (-7.5, 0),
        al simonPegg (7.5, 0),
        al jeanGunnhildr (-7.5, -15),
        al barbaraPegg (7.5, -15)
    ])

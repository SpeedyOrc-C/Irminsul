{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Mondstadt.Gunnhildr where

import Irminsul
import Shortcut
import CommonRelations

fredericaGunnhildr = "FredericaGunnhildr"
simonPegg = "SimonPegg"
jeanGunnhildr = "JeanGunnhildr"

gunnhildr = clusterNode "Gunnhildr" Family
    [
        fredericaGunnhildr,
        simonPegg,
        jeanGunnhildr
    ]
    [
        fredericaGunnhildr `mother` jeanGunnhildr,
        simonPegg `father` jeanGunnhildr,
        jeanGunnhildr `daughter` fredericaGunnhildr
    ]

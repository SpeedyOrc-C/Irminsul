{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.Kaedehara where

import Irminsul
import Shortcut
import CommonRelations

yoshinoriKaedehara = "YoshinoriKaedehara"
hisamichiKaedehara = "HisamichiKaedehara"
kagemitsuKaedehara = "KagemitsuKaedehara"
kageharuKaedehara = "KageharuKaedehara"
kazuhaKaedehara = "KazuhaKaedehara"

kaedehara = clusterLeaf "Kaedehara" Family
    [
        yoshinoriKaedehara,
        hisamichiKaedehara,
        kagemitsuKaedehara,
        kageharuKaedehara,
        kazuhaKaedehara
    ]
    [
        kageharuKaedehara `father` kazuhaKaedehara,
        undefined `father` kageharuKaedehara,
        yoshinoriKaedehara `father` undefined
    ]
    Nothing

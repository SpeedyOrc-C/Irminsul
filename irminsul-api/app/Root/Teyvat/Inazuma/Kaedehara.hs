{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Inazuma.Kaedehara where

import Irminsul
import CommonRelations

yoshinoriKaedehara = "YoshinoriKaedehara"
hisamichiKaedehara = "HisamichiKaedehara"
kagemitsuKaedehara = "KagemitsuKaedehara"
kageharuKaedehara = "KageharuKaedehara"
kazuhaKaedehara = "KazuhaKaedehara"

kageharuKaedeharaFather = "KageharuKaedeharaFather"

kaedehara = clusterLeaf "Kaedehara" Family
    [
        yoshinoriKaedehara,
        hisamichiKaedehara,
        kagemitsuKaedehara,
        kageharuKaedehara,
        kazuhaKaedehara,
        kageharuKaedeharaFather
    ]
    [
        kageharuKaedehara `father` kazuhaKaedehara,
        kageharuKaedeharaFather `father` kageharuKaedehara,
        yoshinoriKaedehara `father` kageharuKaedeharaFather
    ]
    Nothing

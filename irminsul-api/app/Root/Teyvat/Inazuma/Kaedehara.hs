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
        kazuhaKaedehara `son` kageharuKaedehara,
        kageharuKaedeharaFather `father` kageharuKaedehara,
        kageharuKaedehara `son` kageharuKaedeharaFather,
        yoshinoriKaedehara `father` kageharuKaedeharaFather,
        kageharuKaedeharaFather `son` yoshinoriKaedehara
    ]
    (layout (-30, 15) [
        al kazuhaKaedehara (-30, 0),
        al kageharuKaedehara (-15, 0),
        al kageharuKaedeharaFather (0, 0),
        al yoshinoriKaedehara (15, 0)
    ])

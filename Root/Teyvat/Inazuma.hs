{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Inazuma where
-- 稲妻

import Irminsul

eiRaiden = ach "EiRaiden";
    beelzebul = eiRaiden
raidenShogun = ach "RaidenShogun"
makotoRaiden = ach "MakotoRaiden";
    baal = makotoRaiden
kunikuzushiRaiden = ach "KunikuzushiRaiden";
    wanderer = kunikuzushiRaiden;
    scaramouche = kunikuzushiRaiden

ayakaKamisato = ach "AyakaKamisato"
ayatoKamisato = ach "AyatoKamisato"
sayu = ach "Sayu"
mikoYae = ach "MikoYae"
saraKujo = ach "SaraKujo"
shinobuKuki = ach "ShinobuKuki"
ittoArataki = ach "IttoArataki"
yoimiyaNaganohara = ach "YoimiyaNaganohara"
kokomiSangonomiya = ach "KokomiSangonomiya"

inazuma = clusterNode "Inazuma" Country
    [
        eiRaiden,
        raidenShogun,
        makotoRaiden,
        kunikuzushiRaiden,
        ayakaKamisato,
        ayatoKamisato,
        sayu,
        mikoYae,
        saraKujo,
        shinobuKuki,
        ittoArataki,
        yoimiyaNaganohara,
        kokomiSangonomiya
    ]
    [

    ]
    [

    ]

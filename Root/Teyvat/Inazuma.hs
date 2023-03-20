{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Teyvat.Inazuma where
-- 稲妻

import Irminsul

eiRaiden = Atom "EiRaiden" Character;
    beelzebul = eiRaiden
raidenShogun = Atom "RaidenShogun" Character
makotoRaiden = Atom "MakotoRaiden" Character;
    baal = makotoRaiden
kunikuzushiRaiden = Atom "KunikuzushiRaiden" Character;
    wanderer = kunikuzushiRaiden;
    scaramouche = kunikuzushiRaiden

ayakaKamisato = Atom "AyakaKamisato" Character
ayatoKamisato = Atom "AyatoKamisato" Character
sayu = Atom "Sayu" Character
mikoYae = Atom "MikoYae" Character
saraKujo = Atom "SaraKujo" Character
shinobuKuki = Atom "ShinobuKuki" Character
ittoArataki = Atom "IttoArataki" Character
yoimiyaNaganohara = Atom "YoimiyaNaganohara" Character
kokomiSangonomiya = Atom "KokomiSangonomiya" Character

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

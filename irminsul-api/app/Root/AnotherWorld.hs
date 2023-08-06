{-# LANGUAGE OverloadedStrings #-}

module Root.AnotherWorld where
    
import Irminsul
import Shortcut

travellerArchonSide = "TravellerArchonSide"
travellerAbyssSide = "TravellerAbyssSide"
lumine = "Lumine"
aether = "Aether"

anotherWorld = clusterNode "AnotherWorld" World
    [
        travellerArchonSide,
        travellerAbyssSide,
        lumine,
        aether
    ]
    [
    ]
    [
    ]
    Nothing

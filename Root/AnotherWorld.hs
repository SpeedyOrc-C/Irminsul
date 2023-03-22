module Root.AnotherWorld where
    
import Irminsul
import Shortcut

travellerArchonSide = ach "TravellerArchonSide"
travellerAbyssSide = ach "TravellerAbyssSide"
lumine = ach "Lumine"
aether = ach "Aether"

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

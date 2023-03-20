module Root.AnotherWorld where
    
import Irminsul

travellerArchonSide = Atom "TravellerArchonSide" Character
travellerAbyssSide = Atom "TravellerAbyssSide" Character
lumine = Atom "Lumine" Character
aether = Atom "Aether" Character

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

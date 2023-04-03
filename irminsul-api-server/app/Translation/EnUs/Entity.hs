module Translation.EnUs.Entity where

import Data.Maybe

import Irminsul
import Shortcut

import Root.AnotherWorld
import Root.Teyvat.Mondstadt.KnightsOfFavonius
import Root.Teyvat.Kaeya
import Root.Teyvat.Khaenriah
import Root.Teyvat.Mondstadt.DawnWinery
import Root.Teyvat.Mondstadt.KnightsOfFavonius.GuerillaTeam

translationEntityEnUs = [
    ip knightsOfFavonius "Knights of Favonius" [] UntilNow "",
        ip jeanGunnhildr "Jean Gunnhildr" [] UntilNow "",
        ip lisa "Lisa" [] UntilNow "",
        ip kaeya "Kaeya" [] UntilNow "",
            ip kaeyaAlberich "Kaeya Alberich" [] UntilNow "",
            ip kaeyaRagvindr "Kaeya Ragvindr" [] UntilNow "",
        ip amber "Amber" [] UntilNow "",
        ip klee "Klee" [] UntilNow "",
        ip albedo "Albedo" [] UntilNow "",
        ip sucrose "Sucrose" [] UntilNow "",
        ip noelle "Noelle" [] UntilNow "",
        
    ip guerillaTeam "Guerilla Team" [] UntilNow "",
        ip eulaLawrence "Eula Lawrence" [] UntilNow "",
        ip mikaSchmidt "Mika Schmidt" [] UntilNow "",
    
    ip aether "Aether" [] UntilNow "",
    ip lumine "Lumine" [] UntilNow ""
    ]
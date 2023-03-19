{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Gender where
    
import Irminsul (Entity(Cluster))
import Root.Teyvat.Mondstadt (character_Venti)

cluster_Male :: Entity
cluster_Male =
    Cluster "Cluster_Male" males []
    where males = [
            ]

cluster_Female :: Entity
cluster_Female =
    Cluster "Cluster_Female" females []
    where females = [
            ]

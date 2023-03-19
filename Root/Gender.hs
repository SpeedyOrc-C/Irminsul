{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Root.Gender where
    
import Irminsul (Entity(Cluster), ClusterType (Property))

cluster_Male :: Entity
cluster_Male =
    Cluster "Cluster_Male" Property [
    ] []

cluster_Female :: Entity
cluster_Female =
    Cluster "Cluster_Female" Property [
    ] []

module API.All where

import Irminsul
import Root
import Data.JSON
import Data.List (sort)

allClusters :: JSON
allClusters = JArray $ JString <$>
    sort (entityId <$> filter isCluster (elements . entities $ root))

allAtoms :: JSON
allAtoms = JArray $ JString <$>
    sort (entityId <$> filter isAtom (elements . entities $ root))

{-# LANGUAGE OverloadedStrings #-}

module Root.Teyvat.Liyue.WanminRestaurant where
-- 万民堂

import Irminsul
import Shortcut

xiangling = "Xiangling"
guoba = "Guoba";
    marchosius = guoba

wanminRestaurant = clusterLeaf "WanminRestaurant" Organization
    [
        xiangling,
        guoba
    ]
    [

    ]
    Nothing

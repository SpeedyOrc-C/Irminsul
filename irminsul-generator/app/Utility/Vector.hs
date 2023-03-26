module Utility.Vector where

data Vector2 = Vector2 Double Double deriving (Eq, Show)

instance Num Vector2 where
    (+) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1+x2) (y1+y2)
    (-) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1-2) (y1-y2)
    (*) (Vector2 scale _) (Vector2 x y) = Vector2 (scale * x) (scale * y)
    fromInteger x = Vector2 (fromInteger x) 0
    negate (Vector2 x y) = Vector2 (-x) (-y)

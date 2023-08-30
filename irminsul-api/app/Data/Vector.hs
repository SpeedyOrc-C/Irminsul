{-# LANGUAGE OverloadedStrings #-}

module Data.Vector where
import Data.JSON

data Vector2 = Vector2 {x::Double, y::Double} deriving (Eq, Show)

magnitude :: Vector2 -> Double
magnitude (Vector2 x y) = sqrt (x**2 + y**2)
scale :: Double -> Vector2 -> Vector2
scale k (Vector2 x y) = Vector2 (k*x) (k*y)

instance Num Vector2 where
    (+) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1+x2) (y1+y2)
    (-) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1-x2) (y1-y2)
    (*) (Vector2 scale _) (Vector2 x y) = Vector2 (scale * x) (scale * y)
    fromInteger x = Vector2 (fromInteger x) 0
    negate (Vector2 x y) = Vector2 (-x) (-y)

instance ToJSON Vector2 where
    toJSON (Vector2 x y) = JObject [("x", JNumber x), ("y", JNumber y)]

{-|
    Create a Vector2 from a polar coordinate.
    It takes a magnitude and an angle in degree.

    >>> p 2 90
    Vector2 {x = 1.2246063538223773e-16, y = 2.0}

    >>> p 1 45
    Vector2 {x = 0.7071067811865476, y = 0.7071067811865475}
-}
p :: Double -> Double -> Vector2
p len angleDeg = Vector2 (len * cos angle) (len * sin angle) where
    angle = angleDeg * pi / 180

